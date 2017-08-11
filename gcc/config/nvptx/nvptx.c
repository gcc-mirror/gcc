/* Target code for NVPTX.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include <sstream>
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic.h"
#include "alias.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "tm-preds.h"
#include "tm-constrs.h"
#include "langhooks.h"
#include "dbxout.h"
#include "cfgrtl.h"
#include "gimple.h"
#include "stor-layout.h"
#include "builtins.h"
#include "omp-general.h"
#include "omp-low.h"
#include "gomp-constants.h"
#include "dumpfile.h"
#include "internal-fn.h"
#include "gimple-iterator.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-vrp.h"
#include "tree-ssa-operands.h"
#include "tree-ssanames.h"
#include "gimplify.h"
#include "tree-phinodes.h"
#include "cfgloop.h"
#include "fold-const.h"
#include "intl.h"

/* This file should be included last.  */
#include "target-def.h"

#define WORKAROUND_PTXJIT_BUG 1

/* The various PTX memory areas an object might reside in.  */
enum nvptx_data_area
{
  DATA_AREA_GENERIC,
  DATA_AREA_GLOBAL,
  DATA_AREA_SHARED,
  DATA_AREA_LOCAL,
  DATA_AREA_CONST,
  DATA_AREA_PARAM,
  DATA_AREA_MAX
};

/*  We record the data area in the target symbol flags.  */
#define SYMBOL_DATA_AREA(SYM) \
  (nvptx_data_area)((SYMBOL_REF_FLAGS (SYM) >> SYMBOL_FLAG_MACH_DEP_SHIFT) \
		    & 7)
#define SET_SYMBOL_DATA_AREA(SYM,AREA) \
  (SYMBOL_REF_FLAGS (SYM) |= (AREA) << SYMBOL_FLAG_MACH_DEP_SHIFT)

/* Record the function decls we've written, and the libfuncs and function
   decls corresponding to them.  */
static std::stringstream func_decls;

struct declared_libfunc_hasher : ggc_cache_ptr_hash<rtx_def>
{
  static hashval_t hash (rtx x) { return htab_hash_pointer (x); }
  static bool equal (rtx a, rtx b) { return a == b; }
};

static GTY((cache))
  hash_table<declared_libfunc_hasher> *declared_libfuncs_htab;

struct tree_hasher : ggc_cache_ptr_hash<tree_node>
{
  static hashval_t hash (tree t) { return htab_hash_pointer (t); }
  static bool equal (tree a, tree b) { return a == b; }
};

static GTY((cache)) hash_table<tree_hasher> *declared_fndecls_htab;
static GTY((cache)) hash_table<tree_hasher> *needed_fndecls_htab;

/* Buffer needed to broadcast across workers.  This is used for both
   worker-neutering and worker broadcasting.  It is shared by all
   functions emitted.  The buffer is placed in shared memory.  It'd be
   nice if PTX supported common blocks, because then this could be
   shared across TUs (taking the largest size).  */
static unsigned worker_bcast_size;
static unsigned worker_bcast_align;
static GTY(()) rtx worker_bcast_sym;

/* Buffer needed for worker reductions.  This has to be distinct from
   the worker broadcast array, as both may be live concurrently.  */
static unsigned worker_red_size;
static unsigned worker_red_align;
static GTY(()) rtx worker_red_sym;

/* Global lock variable, needed for 128bit worker & gang reductions.  */
static GTY(()) tree global_lock_var;

/* True if any function references __nvptx_stacks.  */
static bool need_softstack_decl;

/* True if any function references __nvptx_uni.  */
static bool need_unisimt_decl;

/* Allocate a new, cleared machine_function structure.  */

static struct machine_function *
nvptx_init_machine_status (void)
{
  struct machine_function *p = ggc_cleared_alloc<machine_function> ();
  p->return_mode = VOIDmode;
  return p;
}

/* Issue a diagnostic when option OPTNAME is enabled (as indicated by OPTVAL)
   and -fopenacc is also enabled.  */

static void
diagnose_openacc_conflict (bool optval, const char *optname)
{
  if (flag_openacc && optval)
    error ("option %s is not supported together with -fopenacc", optname);
}

/* Implement TARGET_OPTION_OVERRIDE.  */

static void
nvptx_option_override (void)
{
  init_machine_status = nvptx_init_machine_status;

  /* Set toplevel_reorder, unless explicitly disabled.  We need
     reordering so that we emit necessary assembler decls of
     undeclared variables. */
  if (!global_options_set.x_flag_toplevel_reorder)
    flag_toplevel_reorder = 1;

  /* Set flag_no_common, unless explicitly disabled.  We fake common
     using .weak, and that's not entirely accurate, so avoid it
     unless forced.  */
  if (!global_options_set.x_flag_no_common)
    flag_no_common = 1;

  /* The patch area requires nops, which we don't have.  */
  if (function_entry_patch_area_size > 0)
    sorry ("not generating patch area, nops not supported");

  /* Assumes that it will see only hard registers.  */
  flag_var_tracking = 0;

  if (nvptx_optimize < 0)
    nvptx_optimize = optimize > 0;

  declared_fndecls_htab = hash_table<tree_hasher>::create_ggc (17);
  needed_fndecls_htab = hash_table<tree_hasher>::create_ggc (17);
  declared_libfuncs_htab
    = hash_table<declared_libfunc_hasher>::create_ggc (17);

  worker_bcast_sym = gen_rtx_SYMBOL_REF (Pmode, "__worker_bcast");
  SET_SYMBOL_DATA_AREA (worker_bcast_sym, DATA_AREA_SHARED);
  worker_bcast_align = GET_MODE_ALIGNMENT (SImode) / BITS_PER_UNIT;

  worker_red_sym = gen_rtx_SYMBOL_REF (Pmode, "__worker_red");
  SET_SYMBOL_DATA_AREA (worker_red_sym, DATA_AREA_SHARED);
  worker_red_align = GET_MODE_ALIGNMENT (SImode) / BITS_PER_UNIT;

  diagnose_openacc_conflict (TARGET_GOMP, "-mgomp");
  diagnose_openacc_conflict (TARGET_SOFT_STACK, "-msoft-stack");
  diagnose_openacc_conflict (TARGET_UNIFORM_SIMT, "-muniform-simt");

  if (TARGET_GOMP)
    target_flags |= MASK_SOFT_STACK | MASK_UNIFORM_SIMT;
}

/* Return a ptx type for MODE.  If PROMOTE, then use .u32 for QImode to
   deal with ptx ideosyncracies.  */

const char *
nvptx_ptx_type_from_mode (machine_mode mode, bool promote)
{
  switch (mode)
    {
    case BLKmode:
      return ".b8";
    case BImode:
      return ".pred";
    case QImode:
      if (promote)
	return ".u32";
      else
	return ".u8";
    case HImode:
      return ".u16";
    case SImode:
      return ".u32";
    case DImode:
      return ".u64";

    case SFmode:
      return ".f32";
    case DFmode:
      return ".f64";

    case V2SImode:
      return ".v2.u32";
    case V2DImode:
      return ".v2.u64";

    default:
      gcc_unreachable ();
    }
}

/* Encode the PTX data area that DECL (which might not actually be a
   _DECL) should reside in.  */

static void
nvptx_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);
  if (first && MEM_P (rtl))
    {
      nvptx_data_area area = DATA_AREA_GENERIC;

      if (TREE_CONSTANT (decl))
	area = DATA_AREA_CONST;
      else if (TREE_CODE (decl) == VAR_DECL)
	{
	  if (lookup_attribute ("shared", DECL_ATTRIBUTES (decl)))
	    {
	      area = DATA_AREA_SHARED;
	      if (DECL_INITIAL (decl))
		error ("static initialization of variable %q+D in %<.shared%>"
		       " memory is not supported", decl);
	    }
	  else
	    area = TREE_READONLY (decl) ? DATA_AREA_CONST : DATA_AREA_GLOBAL;
	}

      SET_SYMBOL_DATA_AREA (XEXP (rtl, 0), area);
    }
}

/* Return the PTX name of the data area in which SYM should be
   placed.  The symbol must have already been processed by
   nvptx_encode_seciton_info, or equivalent.  */

static const char *
section_for_sym (rtx sym)
{
  nvptx_data_area area = SYMBOL_DATA_AREA (sym);
  /* Same order as nvptx_data_area enum.  */
  static char const *const areas[] =
    {"", ".global", ".shared", ".local", ".const", ".param"};

  return areas[area];
}

/* Similarly for a decl.  */

static const char *
section_for_decl (const_tree decl)
{
  return section_for_sym (XEXP (DECL_RTL (CONST_CAST (tree, decl)), 0));
}

/* Check NAME for special function names and redirect them by returning a
   replacement.  This applies to malloc, free and realloc, for which we
   want to use libgcc wrappers, and call, which triggers a bug in
   ptxas.  We can't use TARGET_MANGLE_DECL_ASSEMBLER_NAME, as that's
   not active in an offload compiler -- the names are all set by the
   host-side compiler.  */

static const char *
nvptx_name_replacement (const char *name)
{
  if (strcmp (name, "call") == 0)
    return "__nvptx_call";
  if (strcmp (name, "malloc") == 0)
    return "__nvptx_malloc";
  if (strcmp (name, "free") == 0)
    return "__nvptx_free";
  if (strcmp (name, "realloc") == 0)
    return "__nvptx_realloc";
  return name;
}

/* If MODE should be treated as two registers of an inner mode, return
   that inner mode.  Otherwise return VOIDmode.  */

static machine_mode
maybe_split_mode (machine_mode mode)
{
  if (COMPLEX_MODE_P (mode))
    return GET_MODE_INNER (mode);

  if (mode == TImode)
    return DImode;

  return VOIDmode;
}

/* Return true if mode should be treated as two registers.  */

static bool
split_mode_p (machine_mode mode)
{
  return maybe_split_mode (mode) != VOIDmode;
}

/* Output a register, subreg, or register pair (with optional
   enclosing braces).  */

static void
output_reg (FILE *file, unsigned regno, machine_mode inner_mode,
	    int subreg_offset = -1)
{
  if (inner_mode == VOIDmode)
    {
      if (HARD_REGISTER_NUM_P (regno))
	fprintf (file, "%s", reg_names[regno]);
      else
	fprintf (file, "%%r%d", regno);
    }
  else if (subreg_offset >= 0)
    {
      output_reg (file, regno, VOIDmode);
      fprintf (file, "$%d", subreg_offset);
    }
  else
    {
      if (subreg_offset == -1)
	fprintf (file, "{");
      output_reg (file, regno, inner_mode, GET_MODE_SIZE (inner_mode));
      fprintf (file, ",");
      output_reg (file, regno, inner_mode, 0);
      if (subreg_offset == -1)
	fprintf (file, "}");
    }
}

/* Emit forking instructions for MASK.  */

static void
nvptx_emit_forking (unsigned mask, bool is_call)
{
  mask &= (GOMP_DIM_MASK (GOMP_DIM_WORKER)
	   | GOMP_DIM_MASK (GOMP_DIM_VECTOR));
  if (mask)
    {
      rtx op = GEN_INT (mask | (is_call << GOMP_DIM_MAX));
      
      /* Emit fork at all levels.  This helps form SESE regions, as
	 it creates a block with a single successor before entering a
	 partitooned region.  That is a good candidate for the end of
	 an SESE region.  */
      if (!is_call)
	emit_insn (gen_nvptx_fork (op));
      emit_insn (gen_nvptx_forked (op));
    }
}

/* Emit joining instructions for MASK.  */

static void
nvptx_emit_joining (unsigned mask, bool is_call)
{
  mask &= (GOMP_DIM_MASK (GOMP_DIM_WORKER)
	   | GOMP_DIM_MASK (GOMP_DIM_VECTOR));
  if (mask)
    {
      rtx op = GEN_INT (mask | (is_call << GOMP_DIM_MAX));

      /* Emit joining for all non-call pars to ensure there's a single
	 predecessor for the block the join insn ends up in.  This is
	 needed for skipping entire loops.  */
      if (!is_call)
	emit_insn (gen_nvptx_joining (op));
      emit_insn (gen_nvptx_join (op));
    }
}


/* Determine whether MODE and TYPE (possibly NULL) should be passed or
   returned in memory.  Integer and floating types supported by the
   machine are passed in registers, everything else is passed in
   memory.  Complex types are split.  */

static bool
pass_in_memory (machine_mode mode, const_tree type, bool for_return)
{
  if (type)
    {
      if (AGGREGATE_TYPE_P (type))
	return true;
      if (TREE_CODE (type) == VECTOR_TYPE)
	return true;
    }

  if (!for_return && COMPLEX_MODE_P (mode))
    /* Complex types are passed as two underlying args.  */
    mode = GET_MODE_INNER (mode);

  if (GET_MODE_CLASS (mode) != MODE_INT
      && GET_MODE_CLASS (mode) != MODE_FLOAT)
    return true;

  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return true;

  return false;
}

/* A non-memory argument of mode MODE is being passed, determine the mode it
   should be promoted to.  This is also used for determining return
   type promotion.  */

static machine_mode
promote_arg (machine_mode mode, bool prototyped)
{
  if (!prototyped && mode == SFmode)
    /* K&R float promotion for unprototyped functions.  */
    mode = DFmode;
  else if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (SImode))
    mode = SImode;

  return mode;
}

/* A non-memory return type of MODE is being returned.  Determine the
   mode it should be promoted to.  */

static machine_mode
promote_return (machine_mode mode)
{
  return promote_arg (mode, true);
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
nvptx_function_arg (cumulative_args_t ARG_UNUSED (cum_v), machine_mode mode,
		    const_tree, bool named)
{
  if (mode == VOIDmode || !named)
    return NULL_RTX;

  return gen_reg_rtx (mode);
}

/* Implement TARGET_FUNCTION_INCOMING_ARG.  */

static rtx
nvptx_function_incoming_arg (cumulative_args_t cum_v, machine_mode mode,
			     const_tree, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (mode == VOIDmode || !named)
    return NULL_RTX;

  /* No need to deal with split modes here, the only case that can
     happen is complex modes and those are dealt with by
     TARGET_SPLIT_COMPLEX_ARG.  */
  return gen_rtx_UNSPEC (mode,
			 gen_rtvec (1, GEN_INT (cum->count)),
			 UNSPEC_ARG_REG);
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */

static void
nvptx_function_arg_advance (cumulative_args_t cum_v,
			    machine_mode ARG_UNUSED (mode),
			    const_tree ARG_UNUSED (type),
			    bool ARG_UNUSED (named))
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  cum->count++;
}

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.

   For nvptx This is only used for varadic args.  The type has already
   been promoted and/or converted to invisible reference.  */

static unsigned
nvptx_function_arg_boundary (machine_mode mode, const_tree ARG_UNUSED (type))
{
  return GET_MODE_ALIGNMENT (mode);
}

/* Handle the TARGET_STRICT_ARGUMENT_NAMING target hook.

   For nvptx, we know how to handle functions declared as stdarg: by
   passing an extra pointer to the unnamed arguments.  However, the
   Fortran frontend can produce a different situation, where a
   function pointer is declared with no arguments, but the actual
   function and calls to it take more arguments.  In that case, we
   want to ensure the call matches the definition of the function.  */

static bool
nvptx_strict_argument_naming (cumulative_args_t cum_v)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  return cum->fntype == NULL_TREE || stdarg_p (cum->fntype);
}

/* Implement TARGET_LIBCALL_VALUE.  */

static rtx
nvptx_libcall_value (machine_mode mode, const_rtx)
{
  if (!cfun || !cfun->machine->doing_call)
    /* Pretend to return in a hard reg for early uses before pseudos can be
       generated.  */
    return gen_rtx_REG (mode, NVPTX_RETURN_REGNUM);

  return gen_reg_rtx (mode);
}

/* TARGET_FUNCTION_VALUE implementation.  Returns an RTX representing the place
   where function FUNC returns or receives a value of data type TYPE.  */

static rtx
nvptx_function_value (const_tree type, const_tree ARG_UNUSED (func),
		      bool outgoing)
{
  machine_mode mode = promote_return (TYPE_MODE (type));

  if (outgoing)
    {
      gcc_assert (cfun);
      cfun->machine->return_mode = mode;
      return gen_rtx_REG (mode, NVPTX_RETURN_REGNUM);
    }

  return nvptx_libcall_value (mode, NULL_RTX);
}

/* Implement TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
nvptx_function_value_regno_p (const unsigned int regno)
{
  return regno == NVPTX_RETURN_REGNUM;
}

/* Types with a mode other than those supported by the machine are passed by
   reference in memory.  */

static bool
nvptx_pass_by_reference (cumulative_args_t ARG_UNUSED (cum),
			 machine_mode mode, const_tree type,
			 bool ARG_UNUSED (named))
{
  return pass_in_memory (mode, type, false);
}

/* Implement TARGET_RETURN_IN_MEMORY.  */

static bool
nvptx_return_in_memory (const_tree type, const_tree)
{
  return pass_in_memory (TYPE_MODE (type), type, true);
}

/* Implement TARGET_PROMOTE_FUNCTION_MODE.  */

static machine_mode
nvptx_promote_function_mode (const_tree type, machine_mode mode,
			     int *ARG_UNUSED (punsignedp),
			     const_tree funtype, int for_return)
{
  return promote_arg (mode, for_return || !type || TYPE_ARG_TYPES (funtype));
}

/* Helper for write_arg.  Emit a single PTX argument of MODE, either
   in a prototype, or as copy in a function prologue.  ARGNO is the
   index of this argument in the PTX function.  FOR_REG is negative,
   if we're emitting the PTX prototype.  It is zero if we're copying
   to an argument register and it is greater than zero if we're
   copying to a specific hard register.  */

static int
write_arg_mode (std::stringstream &s, int for_reg, int argno,
		machine_mode mode)
{
  const char *ptx_type = nvptx_ptx_type_from_mode (mode, false);

  if (for_reg < 0)
    {
      /* Writing PTX prototype.  */
      s << (argno ? ", " : " (");
      s << ".param" << ptx_type << " %in_ar" << argno;
    }
  else
    {
      s << "\t.reg" << ptx_type << " ";
      if (for_reg)
	s << reg_names[for_reg];
      else
	s << "%ar" << argno;
      s << ";\n";
      if (argno >= 0)
	{
	  s << "\tld.param" << ptx_type << " ";
	  if (for_reg)
	    s << reg_names[for_reg];
	  else
	    s << "%ar" << argno;
	  s << ", [%in_ar" << argno << "];\n";
	}
    }
  return argno + 1;
}

/* Process function parameter TYPE to emit one or more PTX
   arguments. S, FOR_REG and ARGNO as for write_arg_mode.  PROTOTYPED
   is true, if this is a prototyped function, rather than an old-style
   C declaration.  Returns the next argument number to use.

   The promotion behavior here must match the regular GCC function
   parameter marshalling machinery.  */

static int
write_arg_type (std::stringstream &s, int for_reg, int argno,
		tree type, bool prototyped)
{
  machine_mode mode = TYPE_MODE (type);

  if (mode == VOIDmode)
    return argno;

  if (pass_in_memory (mode, type, false))
    mode = Pmode;
  else
    {
      bool split = TREE_CODE (type) == COMPLEX_TYPE;

      if (split)
	{
	  /* Complex types are sent as two separate args.  */
	  type = TREE_TYPE (type);
	  mode = TYPE_MODE (type);
	  prototyped = true;
	}

      mode = promote_arg (mode, prototyped);
      if (split)
	argno = write_arg_mode (s, for_reg, argno, mode);
    }

  return write_arg_mode (s, for_reg, argno, mode);
}

/* Emit a PTX return as a prototype or function prologue declaration
   for MODE.  */

static void
write_return_mode (std::stringstream &s, bool for_proto, machine_mode mode)
{
  const char *ptx_type = nvptx_ptx_type_from_mode (mode, false);
  const char *pfx = "\t.reg";
  const char *sfx = ";\n";
  
  if (for_proto)
    pfx = "(.param", sfx = "_out) ";
  
  s << pfx << ptx_type << " " << reg_names[NVPTX_RETURN_REGNUM] << sfx;
}

/* Process a function return TYPE to emit a PTX return as a prototype
   or function prologue declaration.  Returns true if return is via an
   additional pointer parameter.  The promotion behavior here must
   match the regular GCC function return mashalling.  */

static bool
write_return_type (std::stringstream &s, bool for_proto, tree type)
{
  machine_mode mode = TYPE_MODE (type);

  if (mode == VOIDmode)
    return false;

  bool return_in_mem = pass_in_memory (mode, type, true);

  if (return_in_mem)
    {
      if (for_proto)
	return return_in_mem;
      
      /* Named return values can cause us to return a pointer as well
	 as expect an argument for the return location.  This is
	 optimization-level specific, so no caller can make use of
	 this data, but more importantly for us, we must ensure it
	 doesn't change the PTX prototype.  */
      mode = (machine_mode) cfun->machine->return_mode;

      if (mode == VOIDmode)
	return return_in_mem;

      /* Clear return_mode to inhibit copy of retval to non-existent
	 retval parameter.  */
      cfun->machine->return_mode = VOIDmode;
    }
  else
    mode = promote_return (mode);

  write_return_mode (s, for_proto, mode);

  return return_in_mem;
}

/* Look for attributes in ATTRS that would indicate we must write a function
   as a .entry kernel rather than a .func.  Return true if one is found.  */

static bool
write_as_kernel (tree attrs)
{
  return (lookup_attribute ("kernel", attrs) != NULL_TREE
	  || (lookup_attribute ("omp target entrypoint", attrs) != NULL_TREE
	      && lookup_attribute ("oacc function", attrs) != NULL_TREE));
  /* For OpenMP target regions, the corresponding kernel entry is emitted from
     write_omp_entry as a separate function.  */
}

/* Emit a linker marker for a function decl or defn.  */

static void
write_fn_marker (std::stringstream &s, bool is_defn, bool globalize,
		 const char *name)
{
  s << "\n// BEGIN";
  if (globalize)
    s << " GLOBAL";
  s << " FUNCTION " << (is_defn ? "DEF: " : "DECL: ");
  s << name << "\n";
}

/* Emit a linker marker for a variable decl or defn.  */

static void
write_var_marker (FILE *file, bool is_defn, bool globalize, const char *name)
{
  fprintf (file, "\n// BEGIN%s VAR %s: ",
	   globalize ? " GLOBAL" : "",
	   is_defn ? "DEF" : "DECL");
  assemble_name_raw (file, name);
  fputs ("\n", file);
}

/* Write a .func or .kernel declaration or definition along with
   a helper comment for use by ld.  S is the stream to write to, DECL
   the decl for the function with name NAME.   For definitions, emit
   a declaration too.  */

static const char *
write_fn_proto (std::stringstream &s, bool is_defn,
		const char *name, const_tree decl)
{
  if (is_defn)
    /* Emit a declaration. The PTX assembler gets upset without it.   */
    name = write_fn_proto (s, false, name, decl);
  else
    {
      /* Avoid repeating the name replacement.  */
      name = nvptx_name_replacement (name);
      if (name[0] == '*')
	name++;
    }

  write_fn_marker (s, is_defn, TREE_PUBLIC (decl), name);

  /* PTX declaration.  */
  if (DECL_EXTERNAL (decl))
    s << ".extern ";
  else if (TREE_PUBLIC (decl))
    s << (DECL_WEAK (decl) ? ".weak " : ".visible ");
  s << (write_as_kernel (DECL_ATTRIBUTES (decl)) ? ".entry " : ".func ");

  tree fntype = TREE_TYPE (decl);
  tree result_type = TREE_TYPE (fntype);

  /* atomic_compare_exchange_$n builtins have an exceptional calling
     convention.  */
  int not_atomic_weak_arg = -1;
  if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (decl))
      {
      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
      case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:
	/* These atomics skip the 'weak' parm in an actual library
	   call.  We must skip it in the prototype too.  */
	not_atomic_weak_arg = 3;
	break;

      default:
	break;
      }

  /* Declare the result.  */
  bool return_in_mem = write_return_type (s, true, result_type);

  s << name;

  int argno = 0;

  /* Emit argument list.  */
  if (return_in_mem)
    argno = write_arg_type (s, -1, argno, ptr_type_node, true);

  /* We get:
     NULL in TYPE_ARG_TYPES, for old-style functions
     NULL in DECL_ARGUMENTS, for builtin functions without another
       declaration.
     So we have to pick the best one we have.  */
  tree args = TYPE_ARG_TYPES (fntype);
  bool prototyped = true;
  if (!args)
    {
      args = DECL_ARGUMENTS (decl);
      prototyped = false;
    }

  for (; args; args = TREE_CHAIN (args), not_atomic_weak_arg--)
    {
      tree type = prototyped ? TREE_VALUE (args) : TREE_TYPE (args);
      
      if (not_atomic_weak_arg)
	argno = write_arg_type (s, -1, argno, type, prototyped);
      else
	gcc_assert (type == boolean_type_node);
    }

  if (stdarg_p (fntype))
    argno = write_arg_type (s, -1, argno, ptr_type_node, true);

  if (DECL_STATIC_CHAIN (decl))
    argno = write_arg_type (s, -1, argno, ptr_type_node, true);

  if (!argno && strcmp (name, "main") == 0)
    {
      argno = write_arg_type (s, -1, argno, integer_type_node, true);
      argno = write_arg_type (s, -1, argno, ptr_type_node, true);
    }

  if (argno)
    s << ")";

  s << (is_defn ? "\n" : ";\n");

  return name;
}

/* Construct a function declaration from a call insn.  This can be
   necessary for two reasons - either we have an indirect call which
   requires a .callprototype declaration, or we have a libcall
   generated by emit_library_call for which no decl exists.  */

static void
write_fn_proto_from_insn (std::stringstream &s, const char *name,
			  rtx result, rtx pat)
{
  if (!name)
    {
      s << "\t.callprototype ";
      name = "_";
    }
  else
    {
      name = nvptx_name_replacement (name);
      write_fn_marker (s, false, true, name);
      s << "\t.extern .func ";
    }

  if (result != NULL_RTX)
    write_return_mode (s, true, GET_MODE (result));

  s << name;

  int arg_end = XVECLEN (pat, 0);
  for (int i = 1; i < arg_end; i++)
    {
      /* We don't have to deal with mode splitting & promotion here,
	 as that was already done when generating the call
	 sequence.  */
      machine_mode mode = GET_MODE (XEXP (XVECEXP (pat, 0, i), 0));

      write_arg_mode (s, -1, i - 1, mode);
    }
  if (arg_end != 1)
    s << ")";
  s << ";\n";
}

/* DECL is an external FUNCTION_DECL, make sure its in the fndecl hash
   table and and write a ptx prototype.  These are emitted at end of
   compilation.  */

static void
nvptx_record_fndecl (tree decl)
{
  tree *slot = declared_fndecls_htab->find_slot (decl, INSERT);
  if (*slot == NULL)
    {
      *slot = decl;
      const char *name = get_fnname_from_decl (decl);
      write_fn_proto (func_decls, false, name, decl);
    }
}

/* Record a libcall or unprototyped external function. CALLEE is the
   SYMBOL_REF.  Insert into the libfunc hash table and emit a ptx
   declaration for it.  */

static void
nvptx_record_libfunc (rtx callee, rtx retval, rtx pat)
{
  rtx *slot = declared_libfuncs_htab->find_slot (callee, INSERT);
  if (*slot == NULL)
    {
      *slot = callee;

      const char *name = XSTR (callee, 0);
      write_fn_proto_from_insn (func_decls, name, retval, pat);
    }
}

/* DECL is an external FUNCTION_DECL, that we're referencing.  If it
   is prototyped, record it now.  Otherwise record it as needed at end
   of compilation, when we might have more information about it.  */

void
nvptx_record_needed_fndecl (tree decl)
{
  if (TYPE_ARG_TYPES (TREE_TYPE (decl)) == NULL_TREE)
    {
      tree *slot = needed_fndecls_htab->find_slot (decl, INSERT);
      if (*slot == NULL)
	*slot = decl;
    }
  else
    nvptx_record_fndecl (decl);
}

/* SYM is a SYMBOL_REF.  If it refers to an external function, record
   it as needed.  */

static void
nvptx_maybe_record_fnsym (rtx sym)
{
  tree decl = SYMBOL_REF_DECL (sym);
  
  if (decl && TREE_CODE (decl) == FUNCTION_DECL && DECL_EXTERNAL (decl))
    nvptx_record_needed_fndecl (decl);
}

/* Emit a local array to hold some part of a conventional stack frame
   and initialize REGNO to point to it.  If the size is zero, it'll
   never be valid to dereference, so we can simply initialize to
   zero.  */

static void
init_frame (FILE  *file, int regno, unsigned align, unsigned size)
{
  if (size)
    fprintf (file, "\t.local .align %d .b8 %s_ar[%u];\n",
	     align, reg_names[regno], size);
  fprintf (file, "\t.reg.u%d %s;\n",
	   POINTER_SIZE, reg_names[regno]);
  fprintf (file, (size ? "\tcvta.local.u%d %s, %s_ar;\n"
		  :  "\tmov.u%d %s, 0;\n"),
	   POINTER_SIZE, reg_names[regno], reg_names[regno]);
}

/* Emit soft stack frame setup sequence.  */

static void
init_softstack_frame (FILE *file, unsigned alignment, HOST_WIDE_INT size)
{
  /* Maintain 64-bit stack alignment.  */
  unsigned keep_align = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
  size = ROUND_UP (size, keep_align);
  int bits = POINTER_SIZE;
  const char *reg_stack = reg_names[STACK_POINTER_REGNUM];
  const char *reg_frame = reg_names[FRAME_POINTER_REGNUM];
  const char *reg_sspslot = reg_names[SOFTSTACK_SLOT_REGNUM];
  const char *reg_sspprev = reg_names[SOFTSTACK_PREV_REGNUM];
  fprintf (file, "\t.reg.u%d %s;\n", bits, reg_stack);
  fprintf (file, "\t.reg.u%d %s;\n", bits, reg_frame);
  fprintf (file, "\t.reg.u%d %s;\n", bits, reg_sspslot);
  fprintf (file, "\t.reg.u%d %s;\n", bits, reg_sspprev);
  fprintf (file, "\t{\n");
  fprintf (file, "\t\t.reg.u32 %%fstmp0;\n");
  fprintf (file, "\t\t.reg.u%d %%fstmp1;\n", bits);
  fprintf (file, "\t\t.reg.u%d %%fstmp2;\n", bits);
  fprintf (file, "\t\tmov.u32 %%fstmp0, %%tid.y;\n");
  fprintf (file, "\t\tmul%s.u32 %%fstmp1, %%fstmp0, %d;\n",
	   bits == 64 ? ".wide" : ".lo", bits / 8);
  fprintf (file, "\t\tmov.u%d %%fstmp2, __nvptx_stacks;\n", bits);

  /* Initialize %sspslot = &__nvptx_stacks[tid.y].  */
  fprintf (file, "\t\tadd.u%d %s, %%fstmp2, %%fstmp1;\n", bits, reg_sspslot);

  /* Initialize %sspprev = __nvptx_stacks[tid.y].  */
  fprintf (file, "\t\tld.shared.u%d %s, [%s];\n",
	   bits, reg_sspprev, reg_sspslot);

  /* Initialize %frame = %sspprev - size.  */
  fprintf (file, "\t\tsub.u%d %s, %s, " HOST_WIDE_INT_PRINT_DEC ";\n",
	   bits, reg_frame, reg_sspprev, size);

  /* Apply alignment, if larger than 64.  */
  if (alignment > keep_align)
    fprintf (file, "\t\tand.b%d %s, %s, %d;\n",
	     bits, reg_frame, reg_frame, -alignment);

  size = crtl->outgoing_args_size;
  gcc_assert (size % keep_align == 0);

  /* Initialize %stack.  */
  fprintf (file, "\t\tsub.u%d %s, %s, " HOST_WIDE_INT_PRINT_DEC ";\n",
	   bits, reg_stack, reg_frame, size);

  if (!crtl->is_leaf)
    fprintf (file, "\t\tst.shared.u%d [%s], %s;\n",
	     bits, reg_sspslot, reg_stack);
  fprintf (file, "\t}\n");
  cfun->machine->has_softstack = true;
  need_softstack_decl = true;
}

/* Emit code to initialize the REGNO predicate register to indicate
   whether we are not lane zero on the NAME axis.  */

static void
nvptx_init_axis_predicate (FILE *file, int regno, const char *name)
{
  fprintf (file, "\t{\n");
  fprintf (file, "\t\t.reg.u32\t%%%s;\n", name);
  fprintf (file, "\t\tmov.u32\t%%%s, %%tid.%s;\n", name, name);
  fprintf (file, "\t\tsetp.ne.u32\t%%r%d, %%%s, 0;\n", regno, name);
  fprintf (file, "\t}\n");
}

/* Emit code to initialize predicate and master lane index registers for
   -muniform-simt code generation variant.  */

static void
nvptx_init_unisimt_predicate (FILE *file)
{
  cfun->machine->unisimt_location = gen_reg_rtx (Pmode);
  int loc = REGNO (cfun->machine->unisimt_location);
  int bits = POINTER_SIZE;
  fprintf (file, "\t.reg.u%d %%r%d;\n", bits, loc);
  fprintf (file, "\t{\n");
  fprintf (file, "\t\t.reg.u32 %%ustmp0;\n");
  fprintf (file, "\t\t.reg.u%d %%ustmp1;\n", bits);
  fprintf (file, "\t\tmov.u32 %%ustmp0, %%tid.y;\n");
  fprintf (file, "\t\tmul%s.u32 %%ustmp1, %%ustmp0, 4;\n",
	   bits == 64 ? ".wide" : ".lo");
  fprintf (file, "\t\tmov.u%d %%r%d, __nvptx_uni;\n", bits, loc);
  fprintf (file, "\t\tadd.u%d %%r%d, %%r%d, %%ustmp1;\n", bits, loc, loc);
  if (cfun->machine->unisimt_predicate)
    {
      int master = REGNO (cfun->machine->unisimt_master);
      int pred = REGNO (cfun->machine->unisimt_predicate);
      fprintf (file, "\t\tld.shared.u32 %%r%d, [%%r%d];\n", master, loc);
      fprintf (file, "\t\tmov.u32 %%ustmp0, %%laneid;\n");
      /* Compute 'master lane index' as 'laneid & __nvptx_uni[tid.y]'.  */
      fprintf (file, "\t\tand.b32 %%r%d, %%r%d, %%ustmp0;\n", master, master);
      /* Compute predicate as 'tid.x == master'.  */
      fprintf (file, "\t\tsetp.eq.u32 %%r%d, %%r%d, %%ustmp0;\n", pred, master);
    }
  fprintf (file, "\t}\n");
  need_unisimt_decl = true;
}

/* Emit kernel NAME for function ORIG outlined for an OpenMP 'target' region:

   extern void gomp_nvptx_main (void (*fn)(void*), void *fnarg);
   void __attribute__((kernel)) NAME (void *arg, char *stack, size_t stacksize)
   {
     __nvptx_stacks[tid.y] = stack + stacksize * (ctaid.x * ntid.y + tid.y + 1);
     __nvptx_uni[tid.y] = 0;
     gomp_nvptx_main (ORIG, arg);
   }
   ORIG itself should not be emitted as a PTX .entry function.  */

static void
write_omp_entry (FILE *file, const char *name, const char *orig)
{
  static bool gomp_nvptx_main_declared;
  if (!gomp_nvptx_main_declared)
    {
      gomp_nvptx_main_declared = true;
      write_fn_marker (func_decls, false, true, "gomp_nvptx_main");
      func_decls << ".extern .func gomp_nvptx_main (.param.u" << POINTER_SIZE
        << " %in_ar1, .param.u" << POINTER_SIZE << " %in_ar2);\n";
    }
  /* PR79332.  Single out this string; it confuses gcc.pot generation.  */
#define NTID_Y "%ntid.y"
#define ENTRY_TEMPLATE(PS, PS_BYTES, MAD_PS_32) "\
 (.param.u" PS " %arg, .param.u" PS " %stack, .param.u" PS " %sz)\n\
{\n\
	.reg.u32 %r<3>;\n\
	.reg.u" PS " %R<4>;\n\
	mov.u32 %r0, %tid.y;\n\
	mov.u32 %r1, " NTID_Y ";\n\
	mov.u32 %r2, %ctaid.x;\n\
	cvt.u" PS ".u32 %R1, %r0;\n\
	" MAD_PS_32 " %R1, %r1, %r2, %R1;\n\
	mov.u" PS " %R0, __nvptx_stacks;\n\
	" MAD_PS_32 " %R0, %r0, " PS_BYTES ", %R0;\n\
	ld.param.u" PS " %R2, [%stack];\n\
	ld.param.u" PS " %R3, [%sz];\n\
	add.u" PS " %R2, %R2, %R3;\n\
	mad.lo.u" PS " %R2, %R1, %R3, %R2;\n\
	st.shared.u" PS " [%R0], %R2;\n\
	mov.u" PS " %R0, __nvptx_uni;\n\
	" MAD_PS_32 " %R0, %r0, 4, %R0;\n\
	mov.u32 %r0, 0;\n\
	st.shared.u32 [%R0], %r0;\n\
	mov.u" PS " %R0, \0;\n\
	ld.param.u" PS " %R1, [%arg];\n\
	{\n\
		.param.u" PS " %P<2>;\n\
		st.param.u" PS " [%P0], %R0;\n\
		st.param.u" PS " [%P1], %R1;\n\
		call.uni gomp_nvptx_main, (%P0, %P1);\n\
	}\n\
	ret.uni;\n\
}\n"
  static const char entry64[] = ENTRY_TEMPLATE ("64", "8", "mad.wide.u32");
  static const char entry32[] = ENTRY_TEMPLATE ("32", "4", "mad.lo.u32  ");
#undef ENTRY_TEMPLATE
#undef NTID_Y
  const char *entry_1 = TARGET_ABI64 ? entry64 : entry32;
  /* Position ENTRY_2 after the embedded nul using strlen of the prefix.  */
  const char *entry_2 = entry_1 + strlen (entry64) + 1;
  fprintf (file, ".visible .entry %s%s%s%s", name, entry_1, orig, entry_2);
  need_softstack_decl = need_unisimt_decl = true;
}

/* Implement ASM_DECLARE_FUNCTION_NAME.  Writes the start of a ptx
   function, including local var decls and copies from the arguments to
   local regs.  */

void
nvptx_declare_function_name (FILE *file, const char *name, const_tree decl)
{
  tree fntype = TREE_TYPE (decl);
  tree result_type = TREE_TYPE (fntype);
  int argno = 0;

  if (lookup_attribute ("omp target entrypoint", DECL_ATTRIBUTES (decl))
      && !lookup_attribute ("oacc function", DECL_ATTRIBUTES (decl)))
    {
      char *buf = (char *) alloca (strlen (name) + sizeof ("$impl"));
      sprintf (buf, "%s$impl", name);
      write_omp_entry (file, name, buf);
      name = buf;
    }
  /* We construct the initial part of the function into a string
     stream, in order to share the prototype writing code.  */
  std::stringstream s;
  write_fn_proto (s, true, name, decl);
  s << "{\n";

  bool return_in_mem = write_return_type (s, false, result_type);
  if (return_in_mem)
    argno = write_arg_type (s, 0, argno, ptr_type_node, true);
  
  /* Declare and initialize incoming arguments.  */
  tree args = TYPE_ARG_TYPES (fntype);
  bool prototyped = true;
  if (!args)
    {
      args = DECL_ARGUMENTS (decl);
      prototyped = false;
    }

  for (; args != NULL_TREE; args = TREE_CHAIN (args))
    {
      tree type = prototyped ? TREE_VALUE (args) : TREE_TYPE (args);

      argno = write_arg_type (s, 0, argno, type, prototyped);
    }

  if (stdarg_p (fntype))
    argno = write_arg_type (s, ARG_POINTER_REGNUM, argno, ptr_type_node,
			    true);

  if (DECL_STATIC_CHAIN (decl) || cfun->machine->has_chain)
    write_arg_type (s, STATIC_CHAIN_REGNUM,
		    DECL_STATIC_CHAIN (decl) ? argno : -1, ptr_type_node,
		    true);

  fprintf (file, "%s", s.str().c_str());

  /* Usually 'crtl->is_leaf' is computed during register allocator
     initialization (which is not done on NVPTX) or for pressure-sensitive
     optimizations.  Initialize it here, except if already set.  */
  if (!crtl->is_leaf)
    crtl->is_leaf = leaf_function_p ();

  HOST_WIDE_INT sz = get_frame_size ();
  bool need_frameptr = sz || cfun->machine->has_chain;
  int alignment = crtl->stack_alignment_needed / BITS_PER_UNIT;
  if (!TARGET_SOFT_STACK)
    {
      /* Declare a local var for outgoing varargs.  */
      if (cfun->machine->has_varadic)
	init_frame (file, STACK_POINTER_REGNUM,
		    UNITS_PER_WORD, crtl->outgoing_args_size);

      /* Declare a local variable for the frame.  Force its size to be
	 DImode-compatible.  */
      if (need_frameptr)
	init_frame (file, FRAME_POINTER_REGNUM, alignment,
		    ROUND_UP (sz, GET_MODE_SIZE (DImode)));
    }
  else if (need_frameptr || cfun->machine->has_varadic || cfun->calls_alloca
	   || (cfun->machine->has_simtreg && !crtl->is_leaf))
    init_softstack_frame (file, alignment, sz);

  if (cfun->machine->has_simtreg)
    {
      unsigned HOST_WIDE_INT &simtsz = cfun->machine->simt_stack_size;
      unsigned HOST_WIDE_INT &align = cfun->machine->simt_stack_align;
      align = MAX (align, GET_MODE_SIZE (DImode));
      if (!crtl->is_leaf || cfun->calls_alloca)
	simtsz = HOST_WIDE_INT_M1U;
      if (simtsz == HOST_WIDE_INT_M1U)
	simtsz = nvptx_softstack_size;
      if (cfun->machine->has_softstack)
	simtsz += POINTER_SIZE / 8;
      simtsz = ROUND_UP (simtsz, GET_MODE_SIZE (DImode));
      if (align > GET_MODE_SIZE (DImode))
	simtsz += align - GET_MODE_SIZE (DImode);
      if (simtsz)
	fprintf (file, "\t.local.align 8 .b8 %%simtstack_ar["
		HOST_WIDE_INT_PRINT_DEC "];\n", simtsz);
    }
  /* Declare the pseudos we have as ptx registers.  */
  int maxregs = max_reg_num ();
  for (int i = LAST_VIRTUAL_REGISTER + 1; i < maxregs; i++)
    {
      if (regno_reg_rtx[i] != const0_rtx)
	{
	  machine_mode mode = PSEUDO_REGNO_MODE (i);
	  machine_mode split = maybe_split_mode (mode);

	  if (split_mode_p (mode))
	    mode = split;
	  fprintf (file, "\t.reg%s ", nvptx_ptx_type_from_mode (mode, true));
	  output_reg (file, i, split, -2);
	  fprintf (file, ";\n");
	}
    }

  /* Emit axis predicates. */
  if (cfun->machine->axis_predicate[0])
    nvptx_init_axis_predicate (file,
			       REGNO (cfun->machine->axis_predicate[0]), "y");
  if (cfun->machine->axis_predicate[1])
    nvptx_init_axis_predicate (file,
			       REGNO (cfun->machine->axis_predicate[1]), "x");
  if (cfun->machine->unisimt_predicate
      || (cfun->machine->has_simtreg && !crtl->is_leaf))
    nvptx_init_unisimt_predicate (file);
}

/* Output code for switching uniform-simt state.  ENTERING indicates whether
   we are entering or leaving non-uniform execution region.  */

static void
nvptx_output_unisimt_switch (FILE *file, bool entering)
{
  if (crtl->is_leaf && !cfun->machine->unisimt_predicate)
    return;
  fprintf (file, "\t{\n");
  fprintf (file, "\t\t.reg.u32 %%ustmp2;\n");
  fprintf (file, "\t\tmov.u32 %%ustmp2, %d;\n", entering ? -1 : 0);
  if (!crtl->is_leaf)
    {
      int loc = REGNO (cfun->machine->unisimt_location);
      fprintf (file, "\t\tst.shared.u32 [%%r%d], %%ustmp2;\n", loc);
    }
  if (cfun->machine->unisimt_predicate)
    {
      int master = REGNO (cfun->machine->unisimt_master);
      int pred = REGNO (cfun->machine->unisimt_predicate);
      fprintf (file, "\t\tmov.u32 %%ustmp2, %%laneid;\n");
      fprintf (file, "\t\tmov.u32 %%r%d, %s;\n",
	       master, entering ? "%ustmp2" : "0");
      fprintf (file, "\t\tsetp.eq.u32 %%r%d, %%r%d, %%ustmp2;\n", pred, master);
    }
  fprintf (file, "\t}\n");
}

/* Output code for allocating per-lane storage and switching soft-stack pointer.
   ENTERING indicates whether we are entering or leaving non-uniform execution.
   PTR is the register pointing to allocated storage, it is assigned to on
   entering and used to restore state on leaving.  SIZE and ALIGN are used only
   on entering.  */

static void
nvptx_output_softstack_switch (FILE *file, bool entering,
			       rtx ptr, rtx size, rtx align)
{
  gcc_assert (REG_P (ptr) && !HARD_REGISTER_P (ptr));
  if (crtl->is_leaf && !cfun->machine->simt_stack_size)
    return;
  int bits = POINTER_SIZE, regno = REGNO (ptr);
  fprintf (file, "\t{\n");
  if (entering)
    {
      fprintf (file, "\t\tcvta.local.u%d %%r%d, %%simtstack_ar + "
	       HOST_WIDE_INT_PRINT_DEC ";\n", bits, regno,
	       cfun->machine->simt_stack_size);
      fprintf (file, "\t\tsub.u%d %%r%d, %%r%d, ", bits, regno, regno);
      if (CONST_INT_P (size))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 ROUND_UP (UINTVAL (size), GET_MODE_SIZE (DImode)));
      else
	output_reg (file, REGNO (size), VOIDmode);
      fputs (";\n", file);
      if (!CONST_INT_P (size) || UINTVAL (align) > GET_MODE_SIZE (DImode))
	fprintf (file,
		 "\t\tand.u%d %%r%d, %%r%d, -" HOST_WIDE_INT_PRINT_DEC ";\n",
		 bits, regno, regno, UINTVAL (align));
    }
  if (cfun->machine->has_softstack)
    {
      const char *reg_stack = reg_names[STACK_POINTER_REGNUM];
      if (entering)
	{
	  fprintf (file, "\t\tst.u%d [%%r%d + -%d], %s;\n",
		   bits, regno, bits / 8, reg_stack);
	  fprintf (file, "\t\tsub.u%d %s, %%r%d, %d;\n",
		   bits, reg_stack, regno, bits / 8);
	}
      else
	{
	  fprintf (file, "\t\tld.u%d %s, [%%r%d + -%d];\n",
		   bits, reg_stack, regno, bits / 8);
	}
      nvptx_output_set_softstack (REGNO (stack_pointer_rtx));
    }
  fprintf (file, "\t}\n");
}

/* Output code to enter non-uniform execution region.  DEST is a register
   to hold a per-lane allocation given by SIZE and ALIGN.  */

const char *
nvptx_output_simt_enter (rtx dest, rtx size, rtx align)
{
  nvptx_output_unisimt_switch (asm_out_file, true);
  nvptx_output_softstack_switch (asm_out_file, true, dest, size, align);
  return "";
}

/* Output code to leave non-uniform execution region.  SRC is the register
   holding per-lane storage previously allocated by omp_simt_enter insn.  */

const char *
nvptx_output_simt_exit (rtx src)
{
  nvptx_output_unisimt_switch (asm_out_file, false);
  nvptx_output_softstack_switch (asm_out_file, false, src, NULL_RTX, NULL_RTX);
  return "";
}

/* Output instruction that sets soft stack pointer in shared memory to the
   value in register given by SRC_REGNO.  */

const char *
nvptx_output_set_softstack (unsigned src_regno)
{
  if (cfun->machine->has_softstack && !crtl->is_leaf)
    {
      fprintf (asm_out_file, "\tst.shared.u%d\t[%s], ",
	       POINTER_SIZE, reg_names[SOFTSTACK_SLOT_REGNUM]);
      output_reg (asm_out_file, src_regno, VOIDmode);
      fprintf (asm_out_file, ";\n");
    }
  return "";
}
/* Output a return instruction.  Also copy the return value to its outgoing
   location.  */

const char *
nvptx_output_return (void)
{
  machine_mode mode = (machine_mode)cfun->machine->return_mode;

  if (mode != VOIDmode)
    fprintf (asm_out_file, "\tst.param%s\t[%s_out], %s;\n",
	     nvptx_ptx_type_from_mode (mode, false),
	     reg_names[NVPTX_RETURN_REGNUM],
	     reg_names[NVPTX_RETURN_REGNUM]);

  return "ret;";
}

/* Terminate a function by writing a closing brace to FILE.  */

void
nvptx_function_end (FILE *file)
{
  fprintf (file, "}\n");
}

/* Decide whether we can make a sibling call to a function.  For ptx, we
   can't.  */

static bool
nvptx_function_ok_for_sibcall (tree, tree)
{
  return false;
}

/* Return Dynamic ReAlignment Pointer RTX.  For PTX there isn't any.  */

static rtx
nvptx_get_drap_rtx (void)
{
  if (TARGET_SOFT_STACK && stack_realign_drap)
    return arg_pointer_rtx;
  return NULL_RTX;
}

/* Implement the TARGET_CALL_ARGS hook.  Record information about one
   argument to the next call.  */

static void
nvptx_call_args (rtx arg, tree fntype)
{
  if (!cfun->machine->doing_call)
    {
      cfun->machine->doing_call = true;
      cfun->machine->is_varadic = false;
      cfun->machine->num_args = 0;

      if (fntype && stdarg_p (fntype))
	{
	  cfun->machine->is_varadic = true;
	  cfun->machine->has_varadic = true;
	  cfun->machine->num_args++;
	}
    }

  if (REG_P (arg) && arg != pc_rtx)
    {
      cfun->machine->num_args++;
      cfun->machine->call_args = alloc_EXPR_LIST (VOIDmode, arg,
						  cfun->machine->call_args);
    }
}

/* Implement the corresponding END_CALL_ARGS hook.  Clear and free the
   information we recorded.  */

static void
nvptx_end_call_args (void)
{
  cfun->machine->doing_call = false;
  free_EXPR_LIST_list (&cfun->machine->call_args);
}

/* Emit the sequence for a call to ADDRESS, setting RETVAL.  Keep
   track of whether calls involving static chains or varargs were seen
   in the current function.
   For libcalls, maintain a hash table of decls we have seen, and
   record a function decl for later when encountering a new one.  */

void
nvptx_expand_call (rtx retval, rtx address)
{
  rtx callee = XEXP (address, 0);
  rtx varargs = NULL_RTX;
  unsigned parallel = 0;

  if (!call_insn_operand (callee, Pmode))
    {
      callee = force_reg (Pmode, callee);
      address = change_address (address, QImode, callee);
    }

  if (GET_CODE (callee) == SYMBOL_REF)
    {
      tree decl = SYMBOL_REF_DECL (callee);
      if (decl != NULL_TREE)
	{
	  if (DECL_STATIC_CHAIN (decl))
	    cfun->machine->has_chain = true;

	  tree attr = oacc_get_fn_attrib (decl);
	  if (attr)
	    {
	      tree dims = TREE_VALUE (attr);

	      parallel = GOMP_DIM_MASK (GOMP_DIM_MAX) - 1;
	      for (int ix = 0; ix != GOMP_DIM_MAX; ix++)
		{
		  if (TREE_PURPOSE (dims)
		      && !integer_zerop (TREE_PURPOSE (dims)))
		    break;
		  /* Not on this axis.  */
		  parallel ^= GOMP_DIM_MASK (ix);
		  dims = TREE_CHAIN (dims);
		}
	    }
	}
    }

  unsigned nargs = cfun->machine->num_args;
  if (cfun->machine->is_varadic)
    {
      varargs = gen_reg_rtx (Pmode);
      emit_move_insn (varargs, stack_pointer_rtx);
    }

  rtvec vec = rtvec_alloc (nargs + 1);
  rtx pat = gen_rtx_PARALLEL (VOIDmode, vec);
  int vec_pos = 0;

  rtx call = gen_rtx_CALL (VOIDmode, address, const0_rtx);
  rtx tmp_retval = retval;
  if (retval)
    {
      if (!nvptx_register_operand (retval, GET_MODE (retval)))
	tmp_retval = gen_reg_rtx (GET_MODE (retval));
      call = gen_rtx_SET (tmp_retval, call);
    }
  XVECEXP (pat, 0, vec_pos++) = call;

  /* Construct the call insn, including a USE for each argument pseudo
     register.  These will be used when printing the insn.  */
  for (rtx arg = cfun->machine->call_args; arg; arg = XEXP (arg, 1))
    XVECEXP (pat, 0, vec_pos++) = gen_rtx_USE (VOIDmode, XEXP (arg, 0));

  if (varargs)
    XVECEXP (pat, 0, vec_pos++) = gen_rtx_USE (VOIDmode, varargs);

  gcc_assert (vec_pos = XVECLEN (pat, 0));

  nvptx_emit_forking (parallel, true);
  emit_call_insn (pat);
  nvptx_emit_joining (parallel, true);

  if (tmp_retval != retval)
    emit_move_insn (retval, tmp_retval);
}

/* Emit a comparison COMPARE, and return the new test to be used in the
   jump.  */

rtx
nvptx_expand_compare (rtx compare)
{
  rtx pred = gen_reg_rtx (BImode);
  rtx cmp = gen_rtx_fmt_ee (GET_CODE (compare), BImode,
			    XEXP (compare, 0), XEXP (compare, 1));
  emit_insn (gen_rtx_SET (pred, cmp));
  return gen_rtx_NE (BImode, pred, const0_rtx);
}

/* Expand the oacc fork & join primitive into ptx-required unspecs.  */

void
nvptx_expand_oacc_fork (unsigned mode)
{
  nvptx_emit_forking (GOMP_DIM_MASK (mode), false);
}

void
nvptx_expand_oacc_join (unsigned mode)
{
  nvptx_emit_joining (GOMP_DIM_MASK (mode), false);
}

/* Generate instruction(s) to unpack a 64 bit object into 2 32 bit
   objects.  */

static rtx
nvptx_gen_unpack (rtx dst0, rtx dst1, rtx src)
{
  rtx res;
  
  switch (GET_MODE (src))
    {
    case DImode:
      res = gen_unpackdisi2 (dst0, dst1, src);
      break;
    case DFmode:
      res = gen_unpackdfsi2 (dst0, dst1, src);
      break;
    default: gcc_unreachable ();
    }
  return res;
}

/* Generate instruction(s) to pack 2 32 bit objects into a 64 bit
   object.  */

static rtx
nvptx_gen_pack (rtx dst, rtx src0, rtx src1)
{
  rtx res;
  
  switch (GET_MODE (dst))
    {
    case DImode:
      res = gen_packsidi2 (dst, src0, src1);
      break;
    case DFmode:
      res = gen_packsidf2 (dst, src0, src1);
      break;
    default: gcc_unreachable ();
    }
  return res;
}

/* Generate an instruction or sequence to broadcast register REG
   across the vectors of a single warp.  */

rtx
nvptx_gen_shuffle (rtx dst, rtx src, rtx idx, nvptx_shuffle_kind kind)
{
  rtx res;

  switch (GET_MODE (dst))
    {
    case SImode:
      res = gen_nvptx_shufflesi (dst, src, idx, GEN_INT (kind));
      break;
    case SFmode:
      res = gen_nvptx_shufflesf (dst, src, idx, GEN_INT (kind));
      break;
    case DImode:
    case DFmode:
      {
	rtx tmp0 = gen_reg_rtx (SImode);
	rtx tmp1 = gen_reg_rtx (SImode);

	start_sequence ();
	emit_insn (nvptx_gen_unpack (tmp0, tmp1, src));
	emit_insn (nvptx_gen_shuffle (tmp0, tmp0, idx, kind));
	emit_insn (nvptx_gen_shuffle (tmp1, tmp1, idx, kind));
	emit_insn (nvptx_gen_pack (dst, tmp0, tmp1));
	res = get_insns ();
	end_sequence ();
      }
      break;
    case BImode:
      {
	rtx tmp = gen_reg_rtx (SImode);
	
	start_sequence ();
	emit_insn (gen_sel_truesi (tmp, src, GEN_INT (1), const0_rtx));
	emit_insn (nvptx_gen_shuffle (tmp, tmp, idx, kind));
	emit_insn (gen_rtx_SET (dst, gen_rtx_NE (BImode, tmp, const0_rtx)));
	res = get_insns ();
	end_sequence ();
      }
      break;
    case QImode:
    case HImode:
      {
	rtx tmp = gen_reg_rtx (SImode);

	start_sequence ();
	emit_insn (gen_rtx_SET (tmp, gen_rtx_fmt_e (ZERO_EXTEND, SImode, src)));
	emit_insn (nvptx_gen_shuffle (tmp, tmp, idx, kind));
	emit_insn (gen_rtx_SET (dst, gen_rtx_fmt_e (TRUNCATE, GET_MODE (dst),
						    tmp)));
	res = get_insns ();
	end_sequence ();
      }
      break;
      
    default:
      gcc_unreachable ();
    }
  return res;
}

/* Generate an instruction or sequence to broadcast register REG
   across the vectors of a single warp.  */

static rtx
nvptx_gen_vcast (rtx reg)
{
  return nvptx_gen_shuffle (reg, reg, const0_rtx, SHUFFLE_IDX);
}

/* Structure used when generating a worker-level spill or fill.  */

struct wcast_data_t
{
  rtx base;  /* Register holding base addr of buffer.  */
  rtx ptr;  /* Iteration var,  if needed.  */
  unsigned offset; /* Offset into worker buffer.  */
};

/* Direction of the spill/fill and looping setup/teardown indicator.  */

enum propagate_mask
  {
    PM_read = 1 << 0,
    PM_write = 1 << 1,
    PM_loop_begin = 1 << 2,
    PM_loop_end = 1 << 3,

    PM_read_write = PM_read | PM_write
  };

/* Generate instruction(s) to spill or fill register REG to/from the
   worker broadcast array.  PM indicates what is to be done, REP
   how many loop iterations will be executed (0 for not a loop).  */
   
static rtx
nvptx_gen_wcast (rtx reg, propagate_mask pm, unsigned rep, wcast_data_t *data)
{
  rtx  res;
  machine_mode mode = GET_MODE (reg);

  switch (mode)
    {
    case BImode:
      {
	rtx tmp = gen_reg_rtx (SImode);
	
	start_sequence ();
	if (pm & PM_read)
	  emit_insn (gen_sel_truesi (tmp, reg, GEN_INT (1), const0_rtx));
	emit_insn (nvptx_gen_wcast (tmp, pm, rep, data));
	if (pm & PM_write)
	  emit_insn (gen_rtx_SET (reg, gen_rtx_NE (BImode, tmp, const0_rtx)));
	res = get_insns ();
	end_sequence ();
      }
      break;

    default:
      {
	rtx addr = data->ptr;

	if (!addr)
	  {
	    unsigned align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;

	    if (align > worker_bcast_align)
	      worker_bcast_align = align;
	    data->offset = (data->offset + align - 1) & ~(align - 1);
	    addr = data->base;
	    if (data->offset)
	      addr = gen_rtx_PLUS (Pmode, addr, GEN_INT (data->offset));
	  }
	
	addr = gen_rtx_MEM (mode, addr);
	if (pm == PM_read)
	  res = gen_rtx_SET (addr, reg);
	else if (pm == PM_write)
	  res = gen_rtx_SET (reg, addr);
	else
	  gcc_unreachable ();

	if (data->ptr)
	  {
	    /* We're using a ptr, increment it.  */
	    start_sequence ();
	    
	    emit_insn (res);
	    emit_insn (gen_adddi3 (data->ptr, data->ptr,
				   GEN_INT (GET_MODE_SIZE (GET_MODE (reg)))));
	    res = get_insns ();
	    end_sequence ();
	  }
	else
	  rep = 1;
	data->offset += rep * GET_MODE_SIZE (GET_MODE (reg));
      }
      break;
    }
  return res;
}

/* Returns true if X is a valid address for use in a memory reference.  */

static bool
nvptx_legitimate_address_p (machine_mode, rtx x, bool)
{
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
      return true;

    case PLUS:
      if (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)))
	return true;
      return false;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    default:
      return false;
    }
}

/* Machinery to output constant initializers.  When beginning an
   initializer, we decide on a fragment size (which is visible in ptx
   in the type used), and then all initializer data is buffered until
   a fragment is filled and ready to be written out.  */

static struct
{
  unsigned HOST_WIDE_INT mask; /* Mask for storing fragment.  */
  unsigned HOST_WIDE_INT val; /* Current fragment value.  */
  unsigned HOST_WIDE_INT remaining; /*  Remaining bytes to be written
					out.  */
  unsigned size;  /* Fragment size to accumulate.  */
  unsigned offset;  /* Offset within current fragment.  */
  bool started;   /* Whether we've output any initializer.  */
} init_frag;

/* The current fragment is full,  write it out.  SYM may provide a
   symbolic reference we should output,  in which case the fragment
   value is the addend.  */

static void
output_init_frag (rtx sym)
{
  fprintf (asm_out_file, init_frag.started ? ", " : " = { ");
  unsigned HOST_WIDE_INT val = init_frag.val;

  init_frag.started = true;
  init_frag.val = 0;
  init_frag.offset = 0;
  init_frag.remaining--;
  
  if (sym)
    {
      fprintf (asm_out_file, "generic(");
      output_address (VOIDmode, sym);
      fprintf (asm_out_file, val ? ") + " : ")");
    }

  if (!sym || val)
    fprintf (asm_out_file, HOST_WIDE_INT_PRINT_DEC, val);
}

/* Add value VAL of size SIZE to the data we're emitting, and keep
   writing out chunks as they fill up.  */

static void
nvptx_assemble_value (unsigned HOST_WIDE_INT val, unsigned size)
{
  val &= ((unsigned  HOST_WIDE_INT)2 << (size * BITS_PER_UNIT - 1)) - 1;

  for (unsigned part = 0; size; size -= part)
    {
      val >>= part * BITS_PER_UNIT;
      part = init_frag.size - init_frag.offset;
      if (part > size)
	part = size;

      unsigned HOST_WIDE_INT partial
	= val << (init_frag.offset * BITS_PER_UNIT);
      init_frag.val |= partial & init_frag.mask;
      init_frag.offset += part;

      if (init_frag.offset == init_frag.size)
	output_init_frag (NULL);
    }
}

/* Target hook for assembling integer object X of size SIZE.  */

static bool
nvptx_assemble_integer (rtx x, unsigned int size, int ARG_UNUSED (aligned_p))
{
  HOST_WIDE_INT val = 0;

  switch (GET_CODE (x))
    {
    default:
      /* Let the generic machinery figure it out, usually for a
	 CONST_WIDE_INT.  */
      return false;

    case CONST_INT:
      nvptx_assemble_value (INTVAL (x), size);
      break;

    case CONST:
      x = XEXP (x, 0);
      gcc_assert (GET_CODE (x) == PLUS);
      val = INTVAL (XEXP (x, 1));
      x = XEXP (x, 0);
      gcc_assert (GET_CODE (x) == SYMBOL_REF);
      /* FALLTHROUGH */

    case SYMBOL_REF:
      gcc_assert (size == init_frag.size);
      if (init_frag.offset)
	sorry ("cannot emit unaligned pointers in ptx assembly");

      nvptx_maybe_record_fnsym (x);
      init_frag.val = val;
      output_init_frag (x);
      break;
    }

  return true;
}

/* Output SIZE zero bytes.  We ignore the FILE argument since the
   functions we're calling to perform the output just use
   asm_out_file.  */

void
nvptx_output_skip (FILE *, unsigned HOST_WIDE_INT size)
{
  /* Finish the current fragment, if it's started.  */
  if (init_frag.offset)
    {
      unsigned part = init_frag.size - init_frag.offset;
      if (part > size)
	part = (unsigned) size;
      size -= part;
      nvptx_assemble_value (0, part);
    }

  /* If this skip doesn't terminate the initializer, write as many
     remaining pieces as possible directly.  */
  if (size < init_frag.remaining * init_frag.size)
    {
      while (size >= init_frag.size)
	{
	  size -= init_frag.size;
	  output_init_frag (NULL_RTX);
	}
      if (size)
	nvptx_assemble_value (0, size);
    }
}

/* Output a string STR with length SIZE.  As in nvptx_output_skip we
   ignore the FILE arg.  */

void
nvptx_output_ascii (FILE *, const char *str, unsigned HOST_WIDE_INT size)
{
  for (unsigned HOST_WIDE_INT i = 0; i < size; i++)
    nvptx_assemble_value (str[i], 1);
}

/* Emit a PTX variable decl and prepare for emission of its
   initializer.  NAME is the symbol name and SETION the PTX data
   area. The type is TYPE, object size SIZE and alignment is ALIGN.
   The caller has already emitted any indentation and linkage
   specifier.  It is responsible for any initializer, terminating ;
   and newline.  SIZE is in bytes, ALIGN is in bits -- confusingly
   this is the opposite way round that PTX wants them!  */

static void
nvptx_assemble_decl_begin (FILE *file, const char *name, const char *section,
			   const_tree type, HOST_WIDE_INT size, unsigned align)
{
  while (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  if (TREE_CODE (type) == VECTOR_TYPE
      || TREE_CODE (type) == COMPLEX_TYPE)
    /* Neither vector nor complex types can contain the other.  */
    type = TREE_TYPE (type);

  unsigned elt_size = int_size_in_bytes (type);

  /* Largest mode we're prepared to accept.  For BLKmode types we
     don't know if it'll contain pointer constants, so have to choose
     pointer size, otherwise we can choose DImode.  */
  machine_mode elt_mode = TYPE_MODE (type) == BLKmode ? Pmode : DImode;

  elt_size |= GET_MODE_SIZE (elt_mode);
  elt_size &= -elt_size; /* Extract LSB set.  */

  init_frag.size = elt_size;
  /* Avoid undefined shift behavior by using '2'.  */
  init_frag.mask = ((unsigned HOST_WIDE_INT)2
		    << (elt_size * BITS_PER_UNIT - 1)) - 1;
  init_frag.val = 0;
  init_frag.offset = 0;
  init_frag.started = false;
  /* Size might not be a multiple of elt size, if there's an
     initialized trailing struct array with smaller type than
     elt_size. */
  init_frag.remaining = (size + elt_size - 1) / elt_size;

  fprintf (file, "%s .align %d .u%d ",
	   section, align / BITS_PER_UNIT,
	   elt_size * BITS_PER_UNIT);
  assemble_name (file, name);

  if (size)
    /* We make everything an array, to simplify any initialization
       emission.  */
    fprintf (file, "[" HOST_WIDE_INT_PRINT_DEC "]", init_frag.remaining);
}

/* Called when the initializer for a decl has been completely output through
   combinations of the three functions above.  */

static void
nvptx_assemble_decl_end (void)
{
  if (init_frag.offset)
    /* This can happen with a packed struct with trailing array member.  */
    nvptx_assemble_value (0, init_frag.size - init_frag.offset);
  fprintf (asm_out_file, init_frag.started ? " };\n" : ";\n");
}

/* Output an uninitialized common or file-scope variable.  */

void
nvptx_output_aligned_decl (FILE *file, const char *name,
			   const_tree decl, HOST_WIDE_INT size, unsigned align)
{
  write_var_marker (file, true, TREE_PUBLIC (decl), name);

  /* If this is public, it is common.  The nearest thing we have to
     common is weak.  */
  fprintf (file, "\t%s", TREE_PUBLIC (decl) ? ".weak " : "");

  nvptx_assemble_decl_begin (file, name, section_for_decl (decl),
			     TREE_TYPE (decl), size, align);
  nvptx_assemble_decl_end ();
}

/* Implement TARGET_ASM_DECLARE_CONSTANT_NAME.  Begin the process of
   writing a constant variable EXP with NAME and SIZE and its
   initializer to FILE.  */

static void
nvptx_asm_declare_constant_name (FILE *file, const char *name,
				 const_tree exp, HOST_WIDE_INT obj_size)
{
  write_var_marker (file, true, false, name);

  fprintf (file, "\t");

  tree type = TREE_TYPE (exp);
  nvptx_assemble_decl_begin (file, name, ".const", type, obj_size,
			     TYPE_ALIGN (type));
}

/* Implement the ASM_DECLARE_OBJECT_NAME macro.  Used to start writing
   a variable DECL with NAME to FILE.  */

void
nvptx_declare_object_name (FILE *file, const char *name, const_tree decl)
{
  write_var_marker (file, true, TREE_PUBLIC (decl), name);

  fprintf (file, "\t%s", (!TREE_PUBLIC (decl) ? ""
			  : DECL_WEAK (decl) ? ".weak " : ".visible "));

  tree type = TREE_TYPE (decl);
  HOST_WIDE_INT obj_size = tree_to_shwi (DECL_SIZE_UNIT (decl));
  nvptx_assemble_decl_begin (file, name, section_for_decl (decl),
			     type, obj_size, DECL_ALIGN (decl));
}

/* Implement TARGET_ASM_GLOBALIZE_LABEL by doing nothing.  */

static void
nvptx_globalize_label (FILE *, const char *)
{
}

/* Implement TARGET_ASM_ASSEMBLE_UNDEFINED_DECL.  Write an extern
   declaration only for variable DECL with NAME to FILE.  */

static void
nvptx_assemble_undefined_decl (FILE *file, const char *name, const_tree decl)
{
  /* The middle end can place constant pool decls into the varpool as
     undefined.  Until that is fixed, catch the problem here.  */
  if (DECL_IN_CONSTANT_POOL (decl))
    return;

  /*  We support weak defintions, and hence have the right
      ASM_WEAKEN_DECL definition.  Diagnose the problem here.  */
  if (DECL_WEAK (decl))
    error_at (DECL_SOURCE_LOCATION (decl),
	      "PTX does not support weak declarations"
	      " (only weak definitions)");
  write_var_marker (file, false, TREE_PUBLIC (decl), name);

  fprintf (file, "\t.extern ");
  tree size = DECL_SIZE_UNIT (decl);
  nvptx_assemble_decl_begin (file, name, section_for_decl (decl),
			     TREE_TYPE (decl), size ? tree_to_shwi (size) : 0,
			     DECL_ALIGN (decl));
  nvptx_assemble_decl_end ();
}

/* Output a pattern for a move instruction.  */

const char *
nvptx_output_mov_insn (rtx dst, rtx src)
{
  machine_mode dst_mode = GET_MODE (dst);
  machine_mode dst_inner = (GET_CODE (dst) == SUBREG
			    ? GET_MODE (XEXP (dst, 0)) : dst_mode);
  machine_mode src_inner = (GET_CODE (src) == SUBREG
			    ? GET_MODE (XEXP (src, 0)) : dst_mode);

  rtx sym = src;
  if (GET_CODE (sym) == CONST)
    sym = XEXP (XEXP (sym, 0), 0);
  if (SYMBOL_REF_P (sym))
    {
      if (SYMBOL_DATA_AREA (sym) != DATA_AREA_GENERIC)
	return "%.\tcvta%D1%t0\t%0, %1;";
      nvptx_maybe_record_fnsym (sym);
    }

  if (src_inner == dst_inner)
    return "%.\tmov%t0\t%0, %1;";

  if (CONSTANT_P (src))
    return (GET_MODE_CLASS (dst_inner) == MODE_INT
	    && GET_MODE_CLASS (src_inner) != MODE_FLOAT
	    ? "%.\tmov%t0\t%0, %1;" : "%.\tmov.b%T0\t%0, %1;");

  if (GET_MODE_SIZE (dst_inner) == GET_MODE_SIZE (src_inner))
    {
      if (GET_MODE_BITSIZE (dst_mode) == 128
	  && GET_MODE_BITSIZE (GET_MODE (src)) == 128)
	{
	  /* mov.b128 is not supported.  */
	  if (dst_inner == V2DImode && src_inner == TImode)
	    return "%.\tmov.u64\t%0.x, %L1;\n\t%.\tmov.u64\t%0.y, %H1;";
	  else if (dst_inner == TImode && src_inner == V2DImode)
	    return "%.\tmov.u64\t%L0, %1.x;\n\t%.\tmov.u64\t%H0, %1.y;";

	  gcc_unreachable ();
	}
      return "%.\tmov.b%T0\t%0, %1;";
    }

  return "%.\tcvt%t0%t1\t%0, %1;";
}

static void nvptx_print_operand (FILE *, rtx, int);

/* Output INSN, which is a call to CALLEE with result RESULT.  For ptx, this
   involves writing .param declarations and in/out copies into them.  For
   indirect calls, also write the .callprototype.  */

const char *
nvptx_output_call_insn (rtx_insn *insn, rtx result, rtx callee)
{
  char buf[16];
  static int labelno;
  bool needs_tgt = register_operand (callee, Pmode);
  rtx pat = PATTERN (insn);
  if (GET_CODE (pat) == COND_EXEC)
    pat = COND_EXEC_CODE (pat);
  int arg_end = XVECLEN (pat, 0);
  tree decl = NULL_TREE;

  fprintf (asm_out_file, "\t{\n");
  if (result != NULL)
    fprintf (asm_out_file, "\t\t.param%s %s_in;\n",
	     nvptx_ptx_type_from_mode (GET_MODE (result), false),
	     reg_names[NVPTX_RETURN_REGNUM]);

  /* Ensure we have a ptx declaration in the output if necessary.  */
  if (GET_CODE (callee) == SYMBOL_REF)
    {
      decl = SYMBOL_REF_DECL (callee);
      if (!decl
	  || (DECL_EXTERNAL (decl) && !TYPE_ARG_TYPES (TREE_TYPE (decl))))
	nvptx_record_libfunc (callee, result, pat);
      else if (DECL_EXTERNAL (decl))
	nvptx_record_fndecl (decl);
    }

  if (needs_tgt)
    {
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCT", labelno);
      labelno++;
      ASM_OUTPUT_LABEL (asm_out_file, buf);
      std::stringstream s;
      write_fn_proto_from_insn (s, NULL, result, pat);
      fputs (s.str().c_str(), asm_out_file);
    }

  for (int argno = 1; argno < arg_end; argno++)
    {
      rtx t = XEXP (XVECEXP (pat, 0, argno), 0);
      machine_mode mode = GET_MODE (t);
      const char *ptx_type = nvptx_ptx_type_from_mode (mode, false);

      /* Mode splitting has already been done.  */
      fprintf (asm_out_file, "\t\t.param%s %%out_arg%d;\n"
	       "\t\tst.param%s [%%out_arg%d], ",
	       ptx_type, argno, ptx_type, argno);
      output_reg (asm_out_file, REGNO (t), VOIDmode);
      fprintf (asm_out_file, ";\n");
    }

  /* The '.' stands for the call's predicate, if any.  */
  nvptx_print_operand (asm_out_file, NULL_RTX, '.');
  fprintf (asm_out_file, "\t\tcall ");
  if (result != NULL_RTX)
    fprintf (asm_out_file, "(%s_in), ", reg_names[NVPTX_RETURN_REGNUM]);
  
  if (decl)
    {
      const char *name = get_fnname_from_decl (decl);
      name = nvptx_name_replacement (name);
      assemble_name (asm_out_file, name);
    }
  else
    output_address (VOIDmode, callee);

  const char *open = "(";
  for (int argno = 1; argno < arg_end; argno++)
    {
      fprintf (asm_out_file, ", %s%%out_arg%d", open, argno);
      open = "";
    }
  if (decl && DECL_STATIC_CHAIN (decl))
    {
      fprintf (asm_out_file, ", %s%s", open, reg_names [STATIC_CHAIN_REGNUM]);
      open = "";
    }
  if (!open[0])
    fprintf (asm_out_file, ")");

  if (needs_tgt)
    {
      fprintf (asm_out_file, ", ");
      assemble_name (asm_out_file, buf);
    }
  fprintf (asm_out_file, ";\n");

  if (find_reg_note (insn, REG_NORETURN, NULL))
    /* No return functions confuse the PTX JIT, as it doesn't realize
       the flow control barrier they imply.  It can seg fault if it
       encounters what looks like an unexitable loop.  Emit a trailing
       trap, which it does grok.  */
    fprintf (asm_out_file, "\t\ttrap; // (noreturn)\n");

  if (result)
    {
      static char rval[sizeof ("\tld.param%%t0\t%%0, [%%%s_in];\n\t}") + 8];

      if (!rval[0])
	/* We must escape the '%' that starts RETURN_REGNUM.  */
	sprintf (rval, "\tld.param%%t0\t%%0, [%%%s_in];\n\t}",
		 reg_names[NVPTX_RETURN_REGNUM]);
      return rval;
    }

  return "}";
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
nvptx_print_operand_punct_valid_p (unsigned char c)
{
  return c == '.' || c== '#';
}

/* Subroutine of nvptx_print_operand; used to print a memory reference X to FILE.  */

static void
nvptx_print_address_operand (FILE *file, rtx x, machine_mode)
{
  rtx off;
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);
  switch (GET_CODE (x))
    {
    case PLUS:
      off = XEXP (x, 1);
      output_address (VOIDmode, XEXP (x, 0));
      fprintf (file, "+");
      output_address (VOIDmode, off);
      break;

    case SYMBOL_REF:
    case LABEL_REF:
      output_addr_const (file, x);
      break;

    default:
      gcc_assert (GET_CODE (x) != MEM);
      nvptx_print_operand (file, x, 0);
      break;
    }
}

/* Write assembly language output for the address ADDR to FILE.  */

static void
nvptx_print_operand_address (FILE *file, machine_mode mode, rtx addr)
{
  nvptx_print_address_operand (file, addr, mode);
}

/* Print an operand, X, to FILE, with an optional modifier in CODE.

   Meaning of CODE:
   . -- print the predicate for the instruction or an emptry string for an
        unconditional one.
   # -- print a rounding mode for the instruction

   A -- print a data area for a MEM
   c -- print an opcode suffix for a comparison operator, including a type code
   D -- print a data area for a MEM operand
   S -- print a shuffle kind specified by CONST_INT
   t -- print a type opcode suffix, promoting QImode to 32 bits
   T -- print a type size in bits
   u -- print a type opcode suffix without promotions.  */

static void
nvptx_print_operand (FILE *file, rtx x, int code)
{
  if (code == '.')
    {
      x = current_insn_predicate;
      if (x)
	{
	  fputs ("@", file);
	  if (GET_CODE (x) == EQ)
	    fputs ("!", file);
	  output_reg (file, REGNO (XEXP (x, 0)), VOIDmode);
	}
      return;
    }
  else if (code == '#')
    {
      fputs (".rn", file);
      return;
    }

  enum rtx_code x_code = GET_CODE (x);
  machine_mode mode = GET_MODE (x);

  switch (code)
    {
    case 'A':
      x = XEXP (x, 0);
      /* FALLTHROUGH.  */

    case 'D':
      if (GET_CODE (x) == CONST)
	x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS)
	x = XEXP (x, 0);

      if (GET_CODE (x) == SYMBOL_REF)
	fputs (section_for_sym (x), file);
      break;

    case 't':
    case 'u':
      if (x_code == SUBREG)
	{
	  machine_mode inner_mode = GET_MODE (SUBREG_REG (x));
	  if (VECTOR_MODE_P (inner_mode)
	      && (GET_MODE_SIZE (mode)
		  <= GET_MODE_SIZE (GET_MODE_INNER (inner_mode))))
	    mode = GET_MODE_INNER (inner_mode);
	  else if (split_mode_p (inner_mode))
	    mode = maybe_split_mode (inner_mode);
	  else
	    mode = inner_mode;
	}
      fprintf (file, "%s", nvptx_ptx_type_from_mode (mode, code == 't'));
      break;

    case 'H':
    case 'L':
      {
	rtx inner_x = SUBREG_REG (x);
	machine_mode inner_mode = GET_MODE (inner_x);
	machine_mode split = maybe_split_mode (inner_mode);

	output_reg (file, REGNO (inner_x), split,
		    (code == 'H'
		     ? GET_MODE_SIZE (inner_mode) / 2
		     : 0));
      }
      break;

    case 'S':
      {
	nvptx_shuffle_kind kind = (nvptx_shuffle_kind) UINTVAL (x);
	/* Same order as nvptx_shuffle_kind.  */
	static const char *const kinds[] = 
	  {".up", ".down", ".bfly", ".idx"};
	fputs (kinds[kind], file);
      }
      break;

    case 'T':
      fprintf (file, "%d", GET_MODE_BITSIZE (mode));
      break;

    case 'j':
      fprintf (file, "@");
      goto common;

    case 'J':
      fprintf (file, "@!");
      goto common;

    case 'c':
      mode = GET_MODE (XEXP (x, 0));
      switch (x_code)
	{
	case EQ:
	  fputs (".eq", file);
	  break;
	case NE:
	  if (FLOAT_MODE_P (mode))
	    fputs (".neu", file);
	  else
	    fputs (".ne", file);
	  break;
	case LE:
	case LEU:
	  fputs (".le", file);
	  break;
	case GE:
	case GEU:
	  fputs (".ge", file);
	  break;
	case LT:
	case LTU:
	  fputs (".lt", file);
	  break;
	case GT:
	case GTU:
	  fputs (".gt", file);
	  break;
	case LTGT:
	  fputs (".ne", file);
	  break;
	case UNEQ:
	  fputs (".equ", file);
	  break;
	case UNLE:
	  fputs (".leu", file);
	  break;
	case UNGE:
	  fputs (".geu", file);
	  break;
	case UNLT:
	  fputs (".ltu", file);
	  break;
	case UNGT:
	  fputs (".gtu", file);
	  break;
	case UNORDERED:
	  fputs (".nan", file);
	  break;
	case ORDERED:
	  fputs (".num", file);
	  break;
	default:
	  gcc_unreachable ();
	}
      if (FLOAT_MODE_P (mode)
	  || x_code == EQ || x_code == NE
	  || x_code == GEU || x_code == GTU
	  || x_code == LEU || x_code == LTU)
	fputs (nvptx_ptx_type_from_mode (mode, true), file);
      else
	fprintf (file, ".s%d", GET_MODE_BITSIZE (mode));
      break;
    default:
    common:
      switch (x_code)
	{
	case SUBREG:
	  {
	    rtx inner_x = SUBREG_REG (x);
	    machine_mode inner_mode = GET_MODE (inner_x);
	    machine_mode split = maybe_split_mode (inner_mode);

	    if (VECTOR_MODE_P (inner_mode)
		&& (GET_MODE_SIZE (mode)
		    <= GET_MODE_SIZE (GET_MODE_INNER (inner_mode))))
	      {
		output_reg (file, REGNO (inner_x), VOIDmode);
		fprintf (file, ".%s", SUBREG_BYTE (x) == 0 ? "x" : "y");
	      }
	    else if (split_mode_p (inner_mode)
		&& (GET_MODE_SIZE (inner_mode) == GET_MODE_SIZE (mode)))
	      output_reg (file, REGNO (inner_x), split);
	    else
	      output_reg (file, REGNO (inner_x), split, SUBREG_BYTE (x));
	  }
	  break;

	case REG:
	  output_reg (file, REGNO (x), maybe_split_mode (mode));
	  break;

	case MEM:
	  fputc ('[', file);
	  nvptx_print_address_operand (file, XEXP (x, 0), mode);
	  fputc (']', file);
	  break;

	case CONST_INT:
	  output_addr_const (file, x);
	  break;

	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  /* We could use output_addr_const, but that can print things like
	     "x-8", which breaks ptxas.  Need to ensure it is output as
	     "x+-8".  */
	  nvptx_print_address_operand (file, x, VOIDmode);
	  break;

	case CONST_DOUBLE:
	  long vals[2];
	  real_to_target (vals, CONST_DOUBLE_REAL_VALUE (x), mode);
	  vals[0] &= 0xffffffff;
	  vals[1] &= 0xffffffff;
	  if (mode == SFmode)
	    fprintf (file, "0f%08lx", vals[0]);
	  else
	    fprintf (file, "0d%08lx%08lx", vals[1], vals[0]);
	  break;

	case CONST_VECTOR:
	  {
	    unsigned n = CONST_VECTOR_NUNITS (x);
	    fprintf (file, "{ ");
	    for (unsigned i = 0; i < n; ++i)
	      {
		if (i != 0)
		  fprintf (file, ", ");

		rtx elem = CONST_VECTOR_ELT (x, i);
		output_addr_const (file, elem);
	      }
	    fprintf (file, " }");
	  }
	  break;

	default:
	  output_addr_const (file, x);
	}
    }
}

/* Record replacement regs used to deal with subreg operands.  */
struct reg_replace
{
  rtx replacement[MAX_RECOG_OPERANDS];
  machine_mode mode;
  int n_allocated;
  int n_in_use;
};

/* Allocate or reuse a replacement in R and return the rtx.  */

static rtx
get_replacement (struct reg_replace *r)
{
  if (r->n_allocated == r->n_in_use)
    r->replacement[r->n_allocated++] = gen_reg_rtx (r->mode);
  return r->replacement[r->n_in_use++];
}

/* Clean up subreg operands.  In ptx assembly, everything is typed, and
   the presence of subregs would break the rules for most instructions.
   Replace them with a suitable new register of the right size, plus
   conversion copyin/copyout instructions.  */

static void
nvptx_reorg_subreg (void)
{
  struct reg_replace qiregs, hiregs, siregs, diregs;
  rtx_insn *insn, *next;

  qiregs.n_allocated = 0;
  hiregs.n_allocated = 0;
  siregs.n_allocated = 0;
  diregs.n_allocated = 0;
  qiregs.mode = QImode;
  hiregs.mode = HImode;
  siregs.mode = SImode;
  diregs.mode = DImode;

  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (!NONDEBUG_INSN_P (insn)
	  || asm_noperands (PATTERN (insn)) >= 0
	  || GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER)
	continue;

      qiregs.n_in_use = 0;
      hiregs.n_in_use = 0;
      siregs.n_in_use = 0;
      diregs.n_in_use = 0;
      extract_insn (insn);
      enum attr_subregs_ok s_ok = get_attr_subregs_ok (insn);

      for (int i = 0; i < recog_data.n_operands; i++)
	{
	  rtx op = recog_data.operand[i];
	  if (GET_CODE (op) != SUBREG)
	    continue;

	  rtx inner = SUBREG_REG (op);

	  machine_mode outer_mode = GET_MODE (op);
	  machine_mode inner_mode = GET_MODE (inner);
	  gcc_assert (s_ok);
	  if (s_ok
	      && (GET_MODE_PRECISION (inner_mode)
		  >= GET_MODE_PRECISION (outer_mode)))
	    continue;
	  gcc_assert (SCALAR_INT_MODE_P (outer_mode));
	  struct reg_replace *r = (outer_mode == QImode ? &qiregs
				   : outer_mode == HImode ? &hiregs
				   : outer_mode == SImode ? &siregs
				   : &diregs);
	  rtx new_reg = get_replacement (r);

	  if (recog_data.operand_type[i] != OP_OUT)
	    {
	      enum rtx_code code;
	      if (GET_MODE_PRECISION (inner_mode)
		  < GET_MODE_PRECISION (outer_mode))
		code = ZERO_EXTEND;
	      else
		code = TRUNCATE;

	      rtx pat = gen_rtx_SET (new_reg,
				     gen_rtx_fmt_e (code, outer_mode, inner));
	      emit_insn_before (pat, insn);
	    }

	  if (recog_data.operand_type[i] != OP_IN)
	    {
	      enum rtx_code code;
	      if (GET_MODE_PRECISION (inner_mode)
		  < GET_MODE_PRECISION (outer_mode))
		code = TRUNCATE;
	      else
		code = ZERO_EXTEND;

	      rtx pat = gen_rtx_SET (inner,
				     gen_rtx_fmt_e (code, inner_mode, new_reg));
	      emit_insn_after (pat, insn);
	    }
	  validate_change (insn, recog_data.operand_loc[i], new_reg, false);
	}
    }
}

/* Return a SImode "master lane index" register for uniform-simt, allocating on
   first use.  */

static rtx
nvptx_get_unisimt_master ()
{
  rtx &master = cfun->machine->unisimt_master;
  return master ? master : master = gen_reg_rtx (SImode);
}

/* Return a BImode "predicate" register for uniform-simt, similar to above.  */

static rtx
nvptx_get_unisimt_predicate ()
{
  rtx &pred = cfun->machine->unisimt_predicate;
  return pred ? pred : pred = gen_reg_rtx (BImode);
}

/* Return true if given call insn references one of the functions provided by
   the CUDA runtime: malloc, free, vprintf.  */

static bool
nvptx_call_insn_is_syscall_p (rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  gcc_checking_assert (GET_CODE (pat) == PARALLEL);
  pat = XVECEXP (pat, 0, 0);
  if (GET_CODE (pat) == SET)
    pat = SET_SRC (pat);
  gcc_checking_assert (GET_CODE (pat) == CALL
		       && GET_CODE (XEXP (pat, 0)) == MEM);
  rtx addr = XEXP (XEXP (pat, 0), 0);
  if (GET_CODE (addr) != SYMBOL_REF)
    return false;
  const char *name = XSTR (addr, 0);
  /* Ordinary malloc/free are redirected to __nvptx_{malloc,free), so only the
     references with forced assembler name refer to PTX syscalls.  For vprintf,
     accept both normal and forced-assembler-name references.  */
  return (!strcmp (name, "vprintf") || !strcmp (name, "*vprintf")
	  || !strcmp (name, "*malloc")
	  || !strcmp (name, "*free"));
}

/* If SET subexpression of INSN sets a register, emit a shuffle instruction to
   propagate its value from lane MASTER to current lane.  */

static void
nvptx_unisimt_handle_set (rtx set, rtx_insn *insn, rtx master)
{
  rtx reg;
  if (GET_CODE (set) == SET && REG_P (reg = SET_DEST (set)))
    emit_insn_after (nvptx_gen_shuffle (reg, reg, master, SHUFFLE_IDX), insn);
}

/* Adjust code for uniform-simt code generation variant by making atomics and
   "syscalls" conditionally executed, and inserting shuffle-based propagation
   for registers being set.  */

static void
nvptx_reorg_uniform_simt ()
{
  rtx_insn *insn, *next;

  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (!(CALL_P (insn) && nvptx_call_insn_is_syscall_p (insn))
	  && !(NONJUMP_INSN_P (insn)
	       && GET_CODE (PATTERN (insn)) == PARALLEL
	       && get_attr_atomic (insn)))
	continue;
      rtx pat = PATTERN (insn);
      rtx master = nvptx_get_unisimt_master ();
      for (int i = 0; i < XVECLEN (pat, 0); i++)
	nvptx_unisimt_handle_set (XVECEXP (pat, 0, i), insn, master);
      rtx pred = nvptx_get_unisimt_predicate ();
      pred = gen_rtx_NE (BImode, pred, const0_rtx);
      pat = gen_rtx_COND_EXEC (VOIDmode, pred, pat);
      validate_change (insn, &PATTERN (insn), pat, false);
    }
}

/* Loop structure of the function.  The entire function is described as
   a NULL loop.  */

struct parallel
{
  /* Parent parallel.  */
  parallel *parent;
  
  /* Next sibling parallel.  */
  parallel *next;

  /* First child parallel.  */
  parallel *inner;

  /* Partitioning mask of the parallel.  */
  unsigned mask;

  /* Partitioning used within inner parallels. */
  unsigned inner_mask;

  /* Location of parallel forked and join.  The forked is the first
     block in the parallel and the join is the first block after of
     the partition.  */
  basic_block forked_block;
  basic_block join_block;

  rtx_insn *forked_insn;
  rtx_insn *join_insn;

  rtx_insn *fork_insn;
  rtx_insn *joining_insn;

  /* Basic blocks in this parallel, but not in child parallels.  The
     FORKED and JOINING blocks are in the partition.  The FORK and JOIN
     blocks are not.  */
  auto_vec<basic_block> blocks;

public:
  parallel (parallel *parent, unsigned mode);
  ~parallel ();
};

/* Constructor links the new parallel into it's parent's chain of
   children.  */

parallel::parallel (parallel *parent_, unsigned mask_)
  :parent (parent_), next (0), inner (0), mask (mask_), inner_mask (0)
{
  forked_block = join_block = 0;
  forked_insn = join_insn = 0;
  fork_insn = joining_insn = 0;
  
  if (parent)
    {
      next = parent->inner;
      parent->inner = this;
    }
}

parallel::~parallel ()
{
  delete inner;
  delete next;
}

/* Map of basic blocks to insns */
typedef hash_map<basic_block, rtx_insn *> bb_insn_map_t;

/* A tuple of an insn of interest and the BB in which it resides.  */
typedef std::pair<rtx_insn *, basic_block> insn_bb_t;
typedef auto_vec<insn_bb_t> insn_bb_vec_t;

/* Split basic blocks such that each forked and join unspecs are at
   the start of their basic blocks.  Thus afterwards each block will
   have a single partitioning mode.  We also do the same for return
   insns, as they are executed by every thread.  Return the
   partitioning mode of the function as a whole.  Populate MAP with
   head and tail blocks.  We also clear the BB visited flag, which is
   used when finding partitions.  */

static void
nvptx_split_blocks (bb_insn_map_t *map)
{
  insn_bb_vec_t worklist;
  basic_block block;
  rtx_insn *insn;

  /* Locate all the reorg instructions of interest.  */
  FOR_ALL_BB_FN (block, cfun)
    {
      bool seen_insn = false;

      /* Clear visited flag, for use by parallel locator  */
      block->flags &= ~BB_VISITED;

      FOR_BB_INSNS (block, insn)
	{
	  if (!INSN_P (insn))
	    continue;
	  switch (recog_memoized (insn))
	    {
	    default:
	      seen_insn = true;
	      continue;
	    case CODE_FOR_nvptx_forked:
	    case CODE_FOR_nvptx_join:
	      break;

	    case CODE_FOR_return:
	      /* We also need to split just before return insns, as
		 that insn needs executing by all threads, but the
		 block it is in probably does not.  */
	      break;
	    }

	  if (seen_insn)
	    /* We've found an instruction that  must be at the start of
	       a block, but isn't.  Add it to the worklist.  */
	    worklist.safe_push (insn_bb_t (insn, block));
	  else
	    /* It was already the first instruction.  Just add it to
	       the map.  */
	    map->get_or_insert (block) = insn;
	  seen_insn = true;
	}
    }

  /* Split blocks on the worklist.  */
  unsigned ix;
  insn_bb_t *elt;
  basic_block remap = 0;
  for (ix = 0; worklist.iterate (ix, &elt); ix++)
    {
      if (remap != elt->second)
	{
	  block = elt->second;
	  remap = block;
	}
      
      /* Split block before insn. The insn is in the new block  */
      edge e = split_block (block, PREV_INSN (elt->first));

      block = e->dest;
      map->get_or_insert (block) = elt->first;
    }
}

/* BLOCK is a basic block containing a head or tail instruction.
   Locate the associated prehead or pretail instruction, which must be
   in the single predecessor block.  */

static rtx_insn *
nvptx_discover_pre (basic_block block, int expected)
{
  gcc_assert (block->preds->length () == 1);
  basic_block pre_block = (*block->preds)[0]->src;
  rtx_insn *pre_insn;

  for (pre_insn = BB_END (pre_block); !INSN_P (pre_insn);
       pre_insn = PREV_INSN (pre_insn))
    gcc_assert (pre_insn != BB_HEAD (pre_block));

  gcc_assert (recog_memoized (pre_insn) == expected);
  return pre_insn;
}

/* Dump this parallel and all its inner parallels.  */

static void
nvptx_dump_pars (parallel *par, unsigned depth)
{
  fprintf (dump_file, "%u: mask %d head=%d, tail=%d\n",
	   depth, par->mask,
	   par->forked_block ? par->forked_block->index : -1,
	   par->join_block ? par->join_block->index : -1);

  fprintf (dump_file, "    blocks:");

  basic_block block;
  for (unsigned ix = 0; par->blocks.iterate (ix, &block); ix++)
    fprintf (dump_file, " %d", block->index);
  fprintf (dump_file, "\n");
  if (par->inner)
    nvptx_dump_pars (par->inner, depth + 1);

  if (par->next)
    nvptx_dump_pars (par->next, depth);
}

/* If BLOCK contains a fork/join marker, process it to create or
   terminate a loop structure.  Add this block to the current loop,
   and then walk successor blocks.   */

static parallel *
nvptx_find_par (bb_insn_map_t *map, parallel *par, basic_block block)
{
  if (block->flags & BB_VISITED)
    return par;
  block->flags |= BB_VISITED;

  if (rtx_insn **endp = map->get (block))
    {
      rtx_insn *end = *endp;

      /* This is a block head or tail, or return instruction.  */
      switch (recog_memoized (end))
	{
	case CODE_FOR_return:
	  /* Return instructions are in their own block, and we
	     don't need to do anything more.  */
	  return par;

	case CODE_FOR_nvptx_forked:
	  /* Loop head, create a new inner loop and add it into
	     our parent's child list.  */
	  {
	    unsigned mask = UINTVAL (XVECEXP (PATTERN (end), 0, 0));

	    gcc_assert (mask);
	    par = new parallel (par, mask);
	    par->forked_block = block;
	    par->forked_insn = end;
	    if (!(mask & GOMP_DIM_MASK (GOMP_DIM_MAX))
		&& (mask & GOMP_DIM_MASK (GOMP_DIM_WORKER)))
	      par->fork_insn
		= nvptx_discover_pre (block, CODE_FOR_nvptx_fork);
	  }
	  break;

	case CODE_FOR_nvptx_join:
	  /* A loop tail.  Finish the current loop and return to
	     parent.  */
	  {
	    unsigned mask = UINTVAL (XVECEXP (PATTERN (end), 0, 0));

	    gcc_assert (par->mask == mask);
	    par->join_block = block;
	    par->join_insn = end;
	    if (!(mask & GOMP_DIM_MASK (GOMP_DIM_MAX))
		&& (mask & GOMP_DIM_MASK (GOMP_DIM_WORKER)))
	      par->joining_insn
		= nvptx_discover_pre (block, CODE_FOR_nvptx_joining);
	    par = par->parent;
	  }
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  if (par)
    /* Add this block onto the current loop's list of blocks.  */
    par->blocks.safe_push (block);
  else
    /* This must be the entry block.  Create a NULL parallel.  */
    par = new parallel (0, 0);

  /* Walk successor blocks.  */
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, block->succs)
    nvptx_find_par (map, par, e->dest);

  return par;
}

/* DFS walk the CFG looking for fork & join markers.  Construct
   loop structures as we go.  MAP is a mapping of basic blocks
   to head & tail markers, discovered when splitting blocks.  This
   speeds up the discovery.  We rely on the BB visited flag having
   been cleared when splitting blocks.  */

static parallel *
nvptx_discover_pars (bb_insn_map_t *map)
{
  basic_block block;

  /* Mark exit blocks as visited.  */
  block = EXIT_BLOCK_PTR_FOR_FN (cfun);
  block->flags |= BB_VISITED;

  /* And entry block as not.  */
  block = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  block->flags &= ~BB_VISITED;

  parallel *par = nvptx_find_par (map, 0, block);

  if (dump_file)
    {
      fprintf (dump_file, "\nLoops\n");
      nvptx_dump_pars (par, 0);
      fprintf (dump_file, "\n");
    }
  
  return par;
}

/* Analyse a group of BBs within a partitioned region and create N
   Single-Entry-Single-Exit regions.  Some of those regions will be
   trivial ones consisting of a single BB.  The blocks of a
   partitioned region might form a set of disjoint graphs -- because
   the region encloses a differently partitoned sub region.

   We use the linear time algorithm described in 'Finding Regions Fast:
   Single Entry Single Exit and control Regions in Linear Time'
   Johnson, Pearson & Pingali.  That algorithm deals with complete
   CFGs, where a back edge is inserted from END to START, and thus the
   problem becomes one of finding equivalent loops.

   In this case we have a partial CFG.  We complete it by redirecting
   any incoming edge to the graph to be from an arbitrary external BB,
   and similarly redirecting any outgoing edge to be to  that BB.
   Thus we end up with a closed graph.

   The algorithm works by building a spanning tree of an undirected
   graph and keeping track of back edges from nodes further from the
   root in the tree to nodes nearer to the root in the tree.  In the
   description below, the root is up and the tree grows downwards.

   We avoid having to deal with degenerate back-edges to the same
   block, by splitting each BB into 3 -- one for input edges, one for
   the node itself and one for the output edges.  Such back edges are
   referred to as 'Brackets'.  Cycle equivalent nodes will have the
   same set of brackets.
   
   Determining bracket equivalency is done by maintaining a list of
   brackets in such a manner that the list length and final bracket
   uniquely identify the set.

   We use coloring to mark all BBs with cycle equivalency with the
   same color.  This is the output of the 'Finding Regions Fast'
   algorithm.  Notice it doesn't actually find the set of nodes within
   a particular region, just unorderd sets of nodes that are the
   entries and exits of SESE regions.
   
   After determining cycle equivalency, we need to find the minimal
   set of SESE regions.  Do this with a DFS coloring walk of the
   complete graph.  We're either 'looking' or 'coloring'.  When
   looking, and we're in the subgraph, we start coloring the color of
   the current node, and remember that node as the start of the
   current color's SESE region.  Every time we go to a new node, we
   decrement the count of nodes with thet color.  If it reaches zero,
   we remember that node as the end of the current color's SESE region
   and return to 'looking'.  Otherwise we color the node the current
   color.

   This way we end up with coloring the inside of non-trivial SESE
   regions with the color of that region.  */

/* A pair of BBs.  We use this to represent SESE regions.  */
typedef std::pair<basic_block, basic_block> bb_pair_t;
typedef auto_vec<bb_pair_t> bb_pair_vec_t;

/* A node in the undirected CFG.  The discriminator SECOND indicates just
   above or just below the BB idicated by FIRST.  */
typedef std::pair<basic_block, int> pseudo_node_t;

/* A bracket indicates an edge towards the root of the spanning tree of the
   undirected graph.  Each bracket has a color, determined
   from the currrent set of brackets.  */
struct bracket
{
  pseudo_node_t back; /* Back target */

  /* Current color and size of set.  */
  unsigned color;
  unsigned size;

  bracket (pseudo_node_t back_)
  : back (back_), color (~0u), size (~0u)
  {
  }

  unsigned get_color (auto_vec<unsigned> &color_counts, unsigned length)
  {
    if (length != size)
      {
	size = length;
	color = color_counts.length ();
	color_counts.quick_push (0);
      }
    color_counts[color]++;
    return color;
  }
};

typedef auto_vec<bracket> bracket_vec_t;

/* Basic block info for finding SESE regions.    */

struct bb_sese
{
  int node;  /* Node number in spanning tree.  */
  int parent; /* Parent node number.  */

  /* The algorithm splits each node A into Ai, A', Ao. The incoming
     edges arrive at pseudo-node Ai and the outgoing edges leave at
     pseudo-node Ao.  We have to remember which way we arrived at a
     particular node when generating the spanning tree.  dir > 0 means
     we arrived at Ai, dir < 0 means we arrived at Ao.  */
  int dir;

  /* Lowest numbered pseudo-node reached via a backedge from thsis
     node, or any descendant.  */
  pseudo_node_t high;

  int color;  /* Cycle-equivalence color  */

  /* Stack of brackets for this node.  */
  bracket_vec_t brackets;

  bb_sese (unsigned node_, unsigned p, int dir_)
  :node (node_), parent (p), dir (dir_)
  {
  }
  ~bb_sese ();

  /* Push a bracket ending at BACK.  */
  void push (const pseudo_node_t &back)
  {
    if (dump_file)
      fprintf (dump_file, "Pushing backedge %d:%+d\n",
	       back.first ? back.first->index : 0, back.second);
    brackets.safe_push (bracket (back));
  }
  
  void append (bb_sese *child);
  void remove (const pseudo_node_t &);

  /* Set node's color.  */
  void set_color (auto_vec<unsigned> &color_counts)
  {
    color = brackets.last ().get_color (color_counts, brackets.length ());
  }
};

bb_sese::~bb_sese ()
{
}

/* Destructively append CHILD's brackets.  */

void
bb_sese::append (bb_sese *child)
{
  if (int len = child->brackets.length ())
    {
      int ix;

      if (dump_file)
	{
	  for (ix = 0; ix < len; ix++)
	    {
	      const pseudo_node_t &pseudo = child->brackets[ix].back;
	      fprintf (dump_file, "Appending (%d)'s backedge %d:%+d\n",
		       child->node, pseudo.first ? pseudo.first->index : 0,
		       pseudo.second);
	    }
	}
      if (!brackets.length ())
	std::swap (brackets, child->brackets);
      else
	{
	  brackets.reserve (len);
	  for (ix = 0; ix < len; ix++)
	    brackets.quick_push (child->brackets[ix]);
	}
    }
}

/* Remove brackets that terminate at PSEUDO.  */

void
bb_sese::remove (const pseudo_node_t &pseudo)
{
  unsigned removed = 0;
  int len = brackets.length ();

  for (int ix = 0; ix < len; ix++)
    {
      if (brackets[ix].back == pseudo)
	{
	  if (dump_file)
	    fprintf (dump_file, "Removing backedge %d:%+d\n",
		     pseudo.first ? pseudo.first->index : 0, pseudo.second);
	  removed++;
	}
      else if (removed)
	brackets[ix-removed] = brackets[ix];
    }
  while (removed--)
    brackets.pop ();
}

/* Accessors for BB's aux pointer.  */
#define BB_SET_SESE(B, S) ((B)->aux = (S))
#define BB_GET_SESE(B) ((bb_sese *)(B)->aux)

/* DFS walk creating SESE data structures.  Only cover nodes with
   BB_VISITED set.  Append discovered blocks to LIST.  We number in
   increments of 3 so that the above and below pseudo nodes can be
   implicitly numbered too.  */

static int
nvptx_sese_number (int n, int p, int dir, basic_block b,
		   auto_vec<basic_block> *list)
{
  if (BB_GET_SESE (b))
    return n;

  if (dump_file)
    fprintf (dump_file, "Block %d(%d), parent (%d), orientation %+d\n",
	     b->index, n, p, dir);
  
  BB_SET_SESE (b, new bb_sese (n, p, dir));
  p = n;
      
  n += 3;
  list->quick_push (b);

  /* First walk the nodes on the 'other side' of this node, then walk
     the nodes on the same side.  */
  for (unsigned ix = 2; ix; ix--)
    {
      vec<edge, va_gc> *edges = dir > 0 ? b->succs : b->preds;
      size_t offset = (dir > 0 ? offsetof (edge_def, dest)
		       : offsetof (edge_def, src));
      edge e;
      edge_iterator (ei);

      FOR_EACH_EDGE (e, ei, edges)
	{
	  basic_block target = *(basic_block *)((char *)e + offset);
	  
	  if (target->flags & BB_VISITED)
	    n = nvptx_sese_number (n, p, dir, target, list);
	}
      dir = -dir;
    }
  return n;
}

/* Process pseudo node above (DIR < 0) or below (DIR > 0) ME.
   EDGES are the outgoing edges and OFFSET is the offset to the src
   or dst block on the edges.   */

static void
nvptx_sese_pseudo (basic_block me, bb_sese *sese, int depth, int dir,
		   vec<edge, va_gc> *edges, size_t offset)
{
  edge e;
  edge_iterator (ei);
  int hi_back = depth;
  pseudo_node_t node_back (0, depth);
  int hi_child = depth;
  pseudo_node_t node_child (0, depth);
  basic_block child = NULL;
  unsigned num_children = 0;
  int usd = -dir * sese->dir;

  if (dump_file)
    fprintf (dump_file, "\nProcessing %d(%d) %+d\n",
	     me->index, sese->node, dir);

  if (dir < 0)
    {
      /* This is the above pseudo-child.  It has the BB itself as an
	 additional child node.  */
      node_child = sese->high;
      hi_child = node_child.second;
      if (node_child.first)
	hi_child += BB_GET_SESE (node_child.first)->node;
      num_children++;
    }

  /* Examine each edge.
     - if it is a child (a) append its bracket list and (b) record
          whether it is the child with the highest reaching bracket.
     - if it is an edge to ancestor, record whether it's the highest
          reaching backlink.  */
  FOR_EACH_EDGE (e, ei, edges)
    {
      basic_block target = *(basic_block *)((char *)e + offset);

      if (bb_sese *t_sese = BB_GET_SESE (target))
	{
	  if (t_sese->parent == sese->node && !(t_sese->dir + usd))
	    {
	      /* Child node.  Append its bracket list. */
	      num_children++;
	      sese->append (t_sese);

	      /* Compare it's hi value.  */
	      int t_hi = t_sese->high.second;

	      if (basic_block child_hi_block = t_sese->high.first)
		t_hi += BB_GET_SESE (child_hi_block)->node;

	      if (hi_child > t_hi)
		{
		  hi_child = t_hi;
		  node_child = t_sese->high;
		  child = target;
		}
	    }
	  else if (t_sese->node < sese->node + dir
		   && !(dir < 0 && sese->parent == t_sese->node))
	    {
	      /* Non-parental ancestor node -- a backlink.  */
	      int d = usd * t_sese->dir;
	      int back = t_sese->node + d;
	
	      if (hi_back > back)
		{
		  hi_back = back;
		  node_back = pseudo_node_t (target, d);
		}
	    }
	}
      else
	{ /* Fallen off graph, backlink to entry node.  */
	  hi_back = 0;
	  node_back = pseudo_node_t (0, 0);
	}
    }

  /* Remove any brackets that terminate at this pseudo node.  */
  sese->remove (pseudo_node_t (me, dir));

  /* Now push any backlinks from this pseudo node.  */
  FOR_EACH_EDGE (e, ei, edges)
    {
      basic_block target = *(basic_block *)((char *)e + offset);
      if (bb_sese *t_sese = BB_GET_SESE (target))
	{
	  if (t_sese->node < sese->node + dir
	      && !(dir < 0 && sese->parent == t_sese->node))
	    /* Non-parental ancestor node - backedge from me.  */
	    sese->push (pseudo_node_t (target, usd * t_sese->dir));
	}
      else
	{
	  /* back edge to entry node */
	  sese->push (pseudo_node_t (0, 0));
	}
    }
  
 /* If this node leads directly or indirectly to a no-return region of
     the graph, then fake a backedge to entry node.  */
  if (!sese->brackets.length () || !edges || !edges->length ())
    {
      hi_back = 0;
      node_back = pseudo_node_t (0, 0);
      sese->push (node_back);
    }

  /* Record the highest reaching backedge from us or a descendant.  */
  sese->high = hi_back < hi_child ? node_back : node_child;

  if (num_children > 1)
    {
      /* There is more than one child -- this is a Y shaped piece of
	 spanning tree.  We have to insert a fake backedge from this
	 node to the highest ancestor reached by not-the-highest
	 reaching child.  Note that there may be multiple children
	 with backedges to the same highest node.  That's ok and we
	 insert the edge to that highest node.  */
      hi_child = depth;
      if (dir < 0 && child)
	{
	  node_child = sese->high;
	  hi_child = node_child.second;
	  if (node_child.first)
	    hi_child += BB_GET_SESE (node_child.first)->node;
	}

      FOR_EACH_EDGE (e, ei, edges)
	{
	  basic_block target = *(basic_block *)((char *)e + offset);

	  if (target == child)
	    /* Ignore the highest child. */
	    continue;

	  bb_sese *t_sese = BB_GET_SESE (target);
	  if (!t_sese)
	    continue;
	  if (t_sese->parent != sese->node)
	    /* Not a child. */
	    continue;

	  /* Compare its hi value.  */
	  int t_hi = t_sese->high.second;

	  if (basic_block child_hi_block = t_sese->high.first)
	    t_hi += BB_GET_SESE (child_hi_block)->node;

	  if (hi_child > t_hi)
	    {
	      hi_child = t_hi;
	      node_child = t_sese->high;
	    }
	}
      
      sese->push (node_child);
    }
}


/* DFS walk of BB graph.  Color node BLOCK according to COLORING then
   proceed to successors.  Set SESE entry and exit nodes of
   REGIONS.  */

static void
nvptx_sese_color (auto_vec<unsigned> &color_counts, bb_pair_vec_t &regions,
		  basic_block block, int coloring)
{
  bb_sese *sese = BB_GET_SESE (block);

  if (block->flags & BB_VISITED)
    {
      /* If we've already encountered this block, either we must not
	 be coloring, or it must have been colored the current color.  */
      gcc_assert (coloring < 0 || (sese && coloring == sese->color));
      return;
    }
  
  block->flags |= BB_VISITED;

  if (sese)
    {
      if (coloring < 0)
	{
	  /* Start coloring a region.  */
	  regions[sese->color].first = block;
	  coloring = sese->color;
	}

      if (!--color_counts[sese->color] && sese->color == coloring)
	{
	  /* Found final block of SESE region.  */
	  regions[sese->color].second = block;
	  coloring = -1;
	}
      else
	/* Color the node, so we can assert on revisiting the node
	   that the graph is indeed SESE.  */
	sese->color = coloring;
    }
  else
    /* Fallen off the subgraph, we cannot be coloring.  */
    gcc_assert (coloring < 0);

  /* Walk each successor block.  */
  if (block->succs && block->succs->length ())
    {
      edge e;
      edge_iterator ei;
      
      FOR_EACH_EDGE (e, ei, block->succs)
	nvptx_sese_color (color_counts, regions, e->dest, coloring);
    }
  else
    gcc_assert (coloring < 0);
}

/* Find minimal set of SESE regions covering BLOCKS.  REGIONS might
   end up with NULL entries in it.  */

static void
nvptx_find_sese (auto_vec<basic_block> &blocks, bb_pair_vec_t &regions)
{
  basic_block block;
  int ix;

  /* First clear each BB of the whole function.  */ 
  FOR_ALL_BB_FN (block, cfun)
    {
      block->flags &= ~BB_VISITED;
      BB_SET_SESE (block, 0);
    }

  /* Mark blocks in the function that are in this graph.  */
  for (ix = 0; blocks.iterate (ix, &block); ix++)
    block->flags |= BB_VISITED;

  /* Counts of nodes assigned to each color.  There cannot be more
     colors than blocks (and hopefully there will be fewer).  */
  auto_vec<unsigned> color_counts;
  color_counts.reserve (blocks.length ());

  /* Worklist of nodes in the spanning tree.  Again, there cannot be
     more nodes in the tree than blocks (there will be fewer if the
     CFG of blocks is disjoint).  */
  auto_vec<basic_block> spanlist;
  spanlist.reserve (blocks.length ());

  /* Make sure every block has its cycle class determined.  */
  for (ix = 0; blocks.iterate (ix, &block); ix++)
    {
      if (BB_GET_SESE (block))
	/* We already met this block in an earlier graph solve.  */
	continue;

      if (dump_file)
	fprintf (dump_file, "Searching graph starting at %d\n", block->index);
      
      /* Number the nodes reachable from block initial DFS order.  */
      int depth = nvptx_sese_number (2, 0, +1, block, &spanlist);

      /* Now walk in reverse DFS order to find cycle equivalents.  */
      while (spanlist.length ())
	{
	  block = spanlist.pop ();
	  bb_sese *sese = BB_GET_SESE (block);

	  /* Do the pseudo node below.  */
	  nvptx_sese_pseudo (block, sese, depth, +1,
			     sese->dir > 0 ? block->succs : block->preds,
			     (sese->dir > 0 ? offsetof (edge_def, dest)
			      : offsetof (edge_def, src)));
	  sese->set_color (color_counts);
	  /* Do the pseudo node above.  */
	  nvptx_sese_pseudo (block, sese, depth, -1,
			     sese->dir < 0 ? block->succs : block->preds,
			     (sese->dir < 0 ? offsetof (edge_def, dest)
			      : offsetof (edge_def, src)));
	}
      if (dump_file)
	fprintf (dump_file, "\n");
    }

  if (dump_file)
    {
      unsigned count;
      const char *comma = "";
      
      fprintf (dump_file, "Found %d cycle equivalents\n",
	       color_counts.length ());
      for (ix = 0; color_counts.iterate (ix, &count); ix++)
	{
	  fprintf (dump_file, "%s%d[%d]={", comma, ix, count);

	  comma = "";
	  for (unsigned jx = 0; blocks.iterate (jx, &block); jx++)
	    if (BB_GET_SESE (block)->color == ix)
	      {
		block->flags |= BB_VISITED;
		fprintf (dump_file, "%s%d", comma, block->index);
		comma=",";
	      }
	  fprintf (dump_file, "}");
	  comma = ", ";
	}
      fprintf (dump_file, "\n");
   }
  
  /* Now we've colored every block in the subgraph.  We now need to
     determine the minimal set of SESE regions that cover that
     subgraph.  Do this with a DFS walk of the complete function.
     During the walk we're either 'looking' or 'coloring'.  When we
     reach the last node of a particular color, we stop coloring and
     return to looking.  */

  /* There cannot be more SESE regions than colors.  */
  regions.reserve (color_counts.length ());
  for (ix = color_counts.length (); ix--;)
    regions.quick_push (bb_pair_t (0, 0));

  for (ix = 0; blocks.iterate (ix, &block); ix++)
    block->flags &= ~BB_VISITED;

  nvptx_sese_color (color_counts, regions, ENTRY_BLOCK_PTR_FOR_FN (cfun), -1);

  if (dump_file)
    {
      const char *comma = "";
      int len = regions.length ();
      
      fprintf (dump_file, "SESE regions:");
      for (ix = 0; ix != len; ix++)
	{
	  basic_block from = regions[ix].first;
	  basic_block to = regions[ix].second;

	  if (from)
	    {
	      fprintf (dump_file, "%s %d{%d", comma, ix, from->index);
	      if (to != from)
		fprintf (dump_file, "->%d", to->index);

	      int color = BB_GET_SESE (from)->color;

	      /* Print the blocks within the region (excluding ends).  */
	      FOR_EACH_BB_FN (block, cfun)
		{
		  bb_sese *sese = BB_GET_SESE (block);

		  if (sese && sese->color == color
		      && block != from && block != to)
		    fprintf (dump_file, ".%d", block->index);
		}
	      fprintf (dump_file, "}");
	    }
	  comma = ",";
	}
      fprintf (dump_file, "\n\n");
    }
  
  for (ix = 0; blocks.iterate (ix, &block); ix++)
    delete BB_GET_SESE (block);
}

#undef BB_SET_SESE
#undef BB_GET_SESE

/* Propagate live state at the start of a partitioned region.  BLOCK
   provides the live register information, and might not contain
   INSN. Propagation is inserted just after INSN. RW indicates whether
   we are reading and/or writing state.  This
   separation is needed for worker-level proppagation where we
   essentially do a spill & fill.  FN is the underlying worker
   function to generate the propagation instructions for single
   register.  DATA is user data.

   We propagate the live register set and the entire frame.  We could
   do better by (a) propagating just the live set that is used within
   the partitioned regions and (b) only propagating stack entries that
   are used.  The latter might be quite hard to determine.  */

typedef rtx (*propagator_fn) (rtx, propagate_mask, unsigned, void *);

static void
nvptx_propagate (basic_block block, rtx_insn *insn, propagate_mask rw,
		 propagator_fn fn, void *data)
{
  bitmap live = DF_LIVE_IN (block);
  bitmap_iterator iterator;
  unsigned ix;

  /* Copy the frame array.  */
  HOST_WIDE_INT fs = get_frame_size ();
  if (fs)
    {
      rtx tmp = gen_reg_rtx (DImode);
      rtx idx = NULL_RTX;
      rtx ptr = gen_reg_rtx (Pmode);
      rtx pred = NULL_RTX;
      rtx_code_label *label = NULL;

      /* The frame size might not be DImode compatible, but the frame
	 array's declaration will be.  So it's ok to round up here.  */
      fs = (fs + GET_MODE_SIZE (DImode) - 1) / GET_MODE_SIZE (DImode);
      /* Detect single iteration loop. */
      if (fs == 1)
	fs = 0;

      start_sequence ();
      emit_insn (gen_rtx_SET (ptr, frame_pointer_rtx));
      if (fs)
	{
	  idx = gen_reg_rtx (SImode);
	  pred = gen_reg_rtx (BImode);
	  label = gen_label_rtx ();
	  
	  emit_insn (gen_rtx_SET (idx, GEN_INT (fs)));
	  /* Allow worker function to initialize anything needed.  */
	  rtx init = fn (tmp, PM_loop_begin, fs, data);
	  if (init)
	    emit_insn (init);
	  emit_label (label);
	  LABEL_NUSES (label)++;
	  emit_insn (gen_addsi3 (idx, idx, GEN_INT (-1)));
	}
      if (rw & PM_read)
	emit_insn (gen_rtx_SET (tmp, gen_rtx_MEM (DImode, ptr)));
      emit_insn (fn (tmp, rw, fs, data));
      if (rw & PM_write)
	emit_insn (gen_rtx_SET (gen_rtx_MEM (DImode, ptr), tmp));
      if (fs)
	{
	  emit_insn (gen_rtx_SET (pred, gen_rtx_NE (BImode, idx, const0_rtx)));
	  emit_insn (gen_adddi3 (ptr, ptr, GEN_INT (GET_MODE_SIZE (DImode))));
	  emit_insn (gen_br_true_uni (pred, label));
	  rtx fini = fn (tmp, PM_loop_end, fs, data);
	  if (fini)
	    emit_insn (fini);
	  emit_insn (gen_rtx_CLOBBER (GET_MODE (idx), idx));
	}
      emit_insn (gen_rtx_CLOBBER (GET_MODE (tmp), tmp));
      emit_insn (gen_rtx_CLOBBER (GET_MODE (ptr), ptr));
      rtx cpy = get_insns ();
      end_sequence ();
      insn = emit_insn_after (cpy, insn);
    }

  /* Copy live registers.  */
  EXECUTE_IF_SET_IN_BITMAP (live, 0, ix, iterator)
    {
      rtx reg = regno_reg_rtx[ix];

      if (REGNO (reg) >= FIRST_PSEUDO_REGISTER)
	{
	  rtx bcast = fn (reg, rw, 0, data);

	  insn = emit_insn_after (bcast, insn);
	}
    }
}

/* Worker for nvptx_vpropagate.  */

static rtx
vprop_gen (rtx reg, propagate_mask pm,
	   unsigned ARG_UNUSED (count), void *ARG_UNUSED (data))
{
  if (!(pm & PM_read_write))
    return 0;
  
  return nvptx_gen_vcast (reg);
}

/* Propagate state that is live at start of BLOCK across the vectors
   of a single warp.  Propagation is inserted just after INSN.   */

static void
nvptx_vpropagate (basic_block block, rtx_insn *insn)
{
  nvptx_propagate (block, insn, PM_read_write, vprop_gen, 0);
}

/* Worker for nvptx_wpropagate.  */

static rtx
wprop_gen (rtx reg, propagate_mask pm, unsigned rep, void *data_)
{
  wcast_data_t *data = (wcast_data_t *)data_;

  if (pm & PM_loop_begin)
    {
      /* Starting a loop, initialize pointer.    */
      unsigned align = GET_MODE_ALIGNMENT (GET_MODE (reg)) / BITS_PER_UNIT;

      if (align > worker_bcast_align)
	worker_bcast_align = align;
      data->offset = (data->offset + align - 1) & ~(align - 1);

      data->ptr = gen_reg_rtx (Pmode);

      return gen_adddi3 (data->ptr, data->base, GEN_INT (data->offset));
    }
  else if (pm & PM_loop_end)
    {
      rtx clobber = gen_rtx_CLOBBER (GET_MODE (data->ptr), data->ptr);
      data->ptr = NULL_RTX;
      return clobber;
    }
  else
    return nvptx_gen_wcast (reg, pm, rep, data);
}

/* Spill or fill live state that is live at start of BLOCK.  PRE_P
   indicates if this is just before partitioned mode (do spill), or
   just after it starts (do fill). Sequence is inserted just after
   INSN.  */

static void
nvptx_wpropagate (bool pre_p, basic_block block, rtx_insn *insn)
{
  wcast_data_t data;

  data.base = gen_reg_rtx (Pmode);
  data.offset = 0;
  data.ptr = NULL_RTX;

  nvptx_propagate (block, insn, pre_p ? PM_read : PM_write, wprop_gen, &data);
  if (data.offset)
    {
      /* Stuff was emitted, initialize the base pointer now.  */
      rtx init = gen_rtx_SET (data.base, worker_bcast_sym);
      emit_insn_after (init, insn);

      if (worker_bcast_size < data.offset)
	worker_bcast_size = data.offset;
    }
}

/* Emit a worker-level synchronization barrier.  We use different
   markers for before and after synchronizations.  */

static rtx
nvptx_wsync (bool after)
{
  return gen_nvptx_barsync (GEN_INT (after));
}

#if WORKAROUND_PTXJIT_BUG
/* Return first real insn in BB, or return NULL_RTX if BB does not contain
   real insns.  */

static rtx_insn *
bb_first_real_insn (basic_block bb)
{
  rtx_insn *insn;

  /* Find first insn of from block.  */
  FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      return insn;

  return 0;
}
#endif

/* Single neutering according to MASK.  FROM is the incoming block and
   TO is the outgoing block.  These may be the same block. Insert at
   start of FROM:
   
     if (tid.<axis>) goto end.

   and insert before ending branch of TO (if there is such an insn):

     end:
     <possibly-broadcast-cond>
     <branch>

   We currently only use differnt FROM and TO when skipping an entire
   loop.  We could do more if we detected superblocks.  */

static void
nvptx_single (unsigned mask, basic_block from, basic_block to)
{
  rtx_insn *head = BB_HEAD (from);
  rtx_insn *tail = BB_END (to);
  unsigned skip_mask = mask;

  while (true)
    {
      /* Find first insn of from block.  */
      while (head != BB_END (from) && !INSN_P (head))
	head = NEXT_INSN (head);

      if (from == to)
	break;

      if (!(JUMP_P (head) && single_succ_p (from)))
	break;

      basic_block jump_target = single_succ (from);
      if (!single_pred_p (jump_target))
	break;

      from = jump_target;
      head = BB_HEAD (from);
    }

  /* Find last insn of to block */
  rtx_insn *limit = from == to ? head : BB_HEAD (to);
  while (tail != limit && !INSN_P (tail) && !LABEL_P (tail))
    tail = PREV_INSN (tail);

  /* Detect if tail is a branch.  */
  rtx tail_branch = NULL_RTX;
  rtx cond_branch = NULL_RTX;
  if (tail && INSN_P (tail))
    {
      tail_branch = PATTERN (tail);
      if (GET_CODE (tail_branch) != SET || SET_DEST (tail_branch) != pc_rtx)
	tail_branch = NULL_RTX;
      else
	{
	  cond_branch = SET_SRC (tail_branch);
	  if (GET_CODE (cond_branch) != IF_THEN_ELSE)
	    cond_branch = NULL_RTX;
	}
    }

  if (tail == head)
    {
      /* If this is empty, do nothing.  */
      if (!head || !INSN_P (head))
	return;

      /* If this is a dummy insn, do nothing.  */
      switch (recog_memoized (head))
	{
	default:
	  break;
	case CODE_FOR_nvptx_fork:
	case CODE_FOR_nvptx_forked:
	case CODE_FOR_nvptx_joining:
	case CODE_FOR_nvptx_join:
	  return;
	}

      if (cond_branch)
	{
	  /* If we're only doing vector single, there's no need to
	     emit skip code because we'll not insert anything.  */
	  if (!(mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR)))
	    skip_mask = 0;
	}
      else if (tail_branch)
	/* Block with only unconditional branch.  Nothing to do.  */
	return;
    }

  /* Insert the vector test inside the worker test.  */
  unsigned mode;
  rtx_insn *before = tail;
  for (mode = GOMP_DIM_WORKER; mode <= GOMP_DIM_VECTOR; mode++)
    if (GOMP_DIM_MASK (mode) & skip_mask)
      {
	rtx_code_label *label = gen_label_rtx ();
	rtx pred = cfun->machine->axis_predicate[mode - GOMP_DIM_WORKER];

	if (!pred)
	  {
	    pred = gen_reg_rtx (BImode);
	    cfun->machine->axis_predicate[mode - GOMP_DIM_WORKER] = pred;
	  }
	
	rtx br;
	if (mode == GOMP_DIM_VECTOR)
	  br = gen_br_true (pred, label);
	else
	  br = gen_br_true_uni (pred, label);
	emit_insn_before (br, head);

	LABEL_NUSES (label)++;
	if (tail_branch)
	  before = emit_label_before (label, before);
	else
	  emit_label_after (label, tail);
      }

  /* Now deal with propagating the branch condition.  */
  if (cond_branch)
    {
      rtx pvar = XEXP (XEXP (cond_branch, 0), 0);

      if (GOMP_DIM_MASK (GOMP_DIM_VECTOR) == mask)
	{
	  /* Vector mode only, do a shuffle.  */
#if WORKAROUND_PTXJIT_BUG
	  /* The branch condition %rcond is propagated like this:

		{
		    .reg .u32 %x;
		    mov.u32 %x,%tid.x;
		    setp.ne.u32 %rnotvzero,%x,0;
		 }

		 @%rnotvzero bra Lskip;
		 setp.<op>.<type> %rcond,op1,op2;
		 Lskip:
		 selp.u32 %rcondu32,1,0,%rcond;
		 shfl.idx.b32 %rcondu32,%rcondu32,0,31;
		 setp.ne.u32 %rcond,%rcondu32,0;

	     There seems to be a bug in the ptx JIT compiler (observed at driver
	     version 381.22, at -O1 and higher for sm_61), that drops the shfl
	     unless %rcond is initialized to something before 'bra Lskip'.  The
	     bug is not observed with ptxas from cuda 8.0.61.

	     It is true that the code is non-trivial: at Lskip, %rcond is
	     uninitialized in threads 1-31, and after the selp the same holds
	     for %rcondu32.  But shfl propagates the defined value in thread 0
	     to threads 1-31, so after the shfl %rcondu32 is defined in threads
	     0-31, and after the setp.ne %rcond is defined in threads 0-31.

	     There is nothing in the PTX spec to suggest that this is wrong, or
	     to explain why the extra initialization is needed.  So, we classify
	     it as a JIT bug, and the extra initialization as workaround.  */
	  emit_insn_before (gen_movbi (pvar, const0_rtx),
			    bb_first_real_insn (from));
#endif
	  emit_insn_before (nvptx_gen_vcast (pvar), tail);
	}
      else
	{
	  /* Includes worker mode, do spill & fill.  By construction
	     we should never have worker mode only. */
	  wcast_data_t data;

	  data.base = worker_bcast_sym;
	  data.ptr = 0;

	  if (worker_bcast_size < GET_MODE_SIZE (SImode))
	    worker_bcast_size = GET_MODE_SIZE (SImode);

	  data.offset = 0;
	  emit_insn_before (nvptx_gen_wcast (pvar, PM_read, 0, &data),
			    before);
	  /* Barrier so other workers can see the write.  */
	  emit_insn_before (nvptx_wsync (false), tail);
	  data.offset = 0;
	  emit_insn_before (nvptx_gen_wcast (pvar, PM_write, 0, &data), tail);
	  /* This barrier is needed to avoid worker zero clobbering
	     the broadcast buffer before all the other workers have
	     had a chance to read this instance of it.  */
	  emit_insn_before (nvptx_wsync (true), tail);
	}

      extract_insn (tail);
      rtx unsp = gen_rtx_UNSPEC (BImode, gen_rtvec (1, pvar),
				 UNSPEC_BR_UNIFIED);
      validate_change (tail, recog_data.operand_loc[0], unsp, false);
    }
}

/* PAR is a parallel that is being skipped in its entirety according to
   MASK.  Treat this as skipping a superblock starting at forked
   and ending at joining.  */

static void
nvptx_skip_par (unsigned mask, parallel *par)
{
  basic_block tail = par->join_block;
  gcc_assert (tail->preds->length () == 1);

  basic_block pre_tail = (*tail->preds)[0]->src;
  gcc_assert (pre_tail->succs->length () == 1);

  nvptx_single (mask, par->forked_block, pre_tail);
}

/* If PAR has a single inner parallel and PAR itself only contains
   empty entry and exit blocks, swallow the inner PAR.  */

static void
nvptx_optimize_inner (parallel *par)
{
  parallel *inner = par->inner;

  /* We mustn't be the outer dummy par.  */
  if (!par->mask)
    return;

  /* We must have a single inner par.  */
  if (!inner || inner->next)
    return;

  /* We must only contain 2 blocks ourselves -- the head and tail of
     the inner par.  */
  if (par->blocks.length () != 2)
    return;

  /* We must be disjoint partitioning.  As we only have vector and
     worker partitioning, this is sufficient to guarantee the pars
     have adjacent partitioning.  */
  if ((par->mask & inner->mask) & (GOMP_DIM_MASK (GOMP_DIM_MAX) - 1))
    /* This indicates malformed code generation.  */
    return;

  /* The outer forked insn should be immediately followed by the inner
     fork insn.  */
  rtx_insn *forked = par->forked_insn;
  rtx_insn *fork = BB_END (par->forked_block);

  if (NEXT_INSN (forked) != fork)
    return;
  gcc_checking_assert (recog_memoized (fork) == CODE_FOR_nvptx_fork);

  /* The outer joining insn must immediately follow the inner join
     insn.  */
  rtx_insn *joining = par->joining_insn;
  rtx_insn *join = inner->join_insn;
  if (NEXT_INSN (join) != joining)
    return;

  /* Preconditions met.  Swallow the inner par.  */
  if (dump_file)
    fprintf (dump_file, "Merging loop %x [%d,%d] into %x [%d,%d]\n",
	     inner->mask, inner->forked_block->index,
	     inner->join_block->index,
	     par->mask, par->forked_block->index, par->join_block->index);

  par->mask |= inner->mask & (GOMP_DIM_MASK (GOMP_DIM_MAX) - 1);

  par->blocks.reserve (inner->blocks.length ());
  while (inner->blocks.length ())
    par->blocks.quick_push (inner->blocks.pop ());

  par->inner = inner->inner;
  inner->inner = NULL;

  delete inner;
}

/* Process the parallel PAR and all its contained
   parallels.  We do everything but the neutering.  Return mask of
   partitioned modes used within this parallel.  */

static unsigned
nvptx_process_pars (parallel *par)
{
  if (nvptx_optimize)
    nvptx_optimize_inner (par);
  
  unsigned inner_mask = par->mask;

  /* Do the inner parallels first.  */
  if (par->inner)
    {
      par->inner_mask = nvptx_process_pars (par->inner);
      inner_mask |= par->inner_mask;
    }

  if (par->mask & GOMP_DIM_MASK (GOMP_DIM_MAX))
    /* No propagation needed for a call.  */;
  else if (par->mask & GOMP_DIM_MASK (GOMP_DIM_WORKER))
    {
      nvptx_wpropagate (false, par->forked_block, par->forked_insn);
      nvptx_wpropagate (true, par->forked_block, par->fork_insn);
      /* Insert begin and end synchronizations.  */
      emit_insn_after (nvptx_wsync (false), par->forked_insn);
      emit_insn_before (nvptx_wsync (true), par->joining_insn);
    }
  else if (par->mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR))
    nvptx_vpropagate (par->forked_block, par->forked_insn);

  /* Now do siblings.  */
  if (par->next)
    inner_mask |= nvptx_process_pars (par->next);
  return inner_mask;
}

/* Neuter the parallel described by PAR.  We recurse in depth-first
   order.  MODES are the partitioning of the execution and OUTER is
   the partitioning of the parallels we are contained in.  */

static void
nvptx_neuter_pars (parallel *par, unsigned modes, unsigned outer)
{
  unsigned me = (par->mask
		 & (GOMP_DIM_MASK (GOMP_DIM_WORKER)
		    | GOMP_DIM_MASK (GOMP_DIM_VECTOR)));
  unsigned  skip_mask = 0, neuter_mask = 0;
  
  if (par->inner)
    nvptx_neuter_pars (par->inner, modes, outer | me);

  for (unsigned mode = GOMP_DIM_WORKER; mode <= GOMP_DIM_VECTOR; mode++)
    {
      if ((outer | me) & GOMP_DIM_MASK (mode))
	{} /* Mode is partitioned: no neutering.  */
      else if (!(modes & GOMP_DIM_MASK (mode)))
	{} /* Mode is not used: nothing to do.  */
      else if (par->inner_mask & GOMP_DIM_MASK (mode)
	       || !par->forked_insn)
	/* Partitioned in inner parallels, or we're not a partitioned
	   at all: neuter individual blocks.  */
	neuter_mask |= GOMP_DIM_MASK (mode);
      else if (!par->parent || !par->parent->forked_insn
	       || par->parent->inner_mask & GOMP_DIM_MASK (mode))
	/* Parent isn't a parallel or contains this paralleling: skip
	   parallel at this level.  */
	skip_mask |= GOMP_DIM_MASK (mode);
      else
	{} /* Parent will skip this parallel itself.  */
    }

  if (neuter_mask)
    {
      int ix, len;

      if (nvptx_optimize)
	{
	  /* Neuter whole SESE regions.  */
	  bb_pair_vec_t regions;

	  nvptx_find_sese (par->blocks, regions);
	  len = regions.length ();
	  for (ix = 0; ix != len; ix++)
	    {
	      basic_block from = regions[ix].first;
	      basic_block to = regions[ix].second;

	      if (from)
		nvptx_single (neuter_mask, from, to);
	      else
		gcc_assert (!to);
	    }
	}
      else
	{
	  /* Neuter each BB individually.  */
	  len = par->blocks.length ();
	  for (ix = 0; ix != len; ix++)
	    {
	      basic_block block = par->blocks[ix];

	      nvptx_single (neuter_mask, block, block);
	    }
	}
    }

  if (skip_mask)
      nvptx_skip_par (skip_mask, par);
  
  if (par->next)
    nvptx_neuter_pars (par->next, modes, outer);
}

/* PTX-specific reorganization
   - Split blocks at fork and join instructions
   - Compute live registers
   - Mark now-unused registers, so function begin doesn't declare
   unused registers.
   - Insert state propagation when entering partitioned mode
   - Insert neutering instructions when in single mode
   - Replace subregs with suitable sequences.
*/

static void
nvptx_reorg (void)
{
  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  thread_prologue_and_epilogue_insns ();

  /* Split blocks and record interesting unspecs.  */
  bb_insn_map_t bb_insn_map;

  nvptx_split_blocks (&bb_insn_map);

  /* Compute live regs */
  df_clear_flags (DF_LR_RUN_DCE);
  df_set_flags (DF_NO_INSN_RESCAN | DF_NO_HARD_REGS);
  df_live_add_problem ();
  df_live_set_all_dirty ();
  df_analyze ();
  regstat_init_n_sets_and_refs ();

  if (dump_file)
    df_dump (dump_file);
  
  /* Mark unused regs as unused.  */
  int max_regs = max_reg_num ();
  for (int i = LAST_VIRTUAL_REGISTER + 1; i < max_regs; i++)
    if (REG_N_SETS (i) == 0 && REG_N_REFS (i) == 0)
      regno_reg_rtx[i] = const0_rtx;

  /* Determine launch dimensions of the function.  If it is not an
     offloaded function  (i.e. this is a regular compiler), the
     function has no neutering.  */
  tree attr = oacc_get_fn_attrib (current_function_decl);
  if (attr)
    {
      /* If we determined this mask before RTL expansion, we could
	 elide emission of some levels of forks and joins.  */
      unsigned mask = 0;
      tree dims = TREE_VALUE (attr);
      unsigned ix;

      for (ix = 0; ix != GOMP_DIM_MAX; ix++, dims = TREE_CHAIN (dims))
	{
	  int size = TREE_INT_CST_LOW (TREE_VALUE (dims));
	  tree allowed = TREE_PURPOSE (dims);

	  if (size != 1 && !(allowed && integer_zerop (allowed)))
	    mask |= GOMP_DIM_MASK (ix);
	}
      /* If there is worker neutering, there must be vector
	 neutering.  Otherwise the hardware will fail.  */
      gcc_assert (!(mask & GOMP_DIM_MASK (GOMP_DIM_WORKER))
		  || (mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR)));

      /* Discover & process partitioned regions.  */
      parallel *pars = nvptx_discover_pars (&bb_insn_map);
      nvptx_process_pars (pars);
      nvptx_neuter_pars (pars, mask, 0);
      delete pars;
    }

  /* Replace subregs.  */
  nvptx_reorg_subreg ();

  if (TARGET_UNIFORM_SIMT)
    nvptx_reorg_uniform_simt ();

  regstat_free_n_sets_and_refs ();

  df_finish_pass (true);
}

/* Handle a "kernel" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
nvptx_handle_kernel_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error ("%qE attribute only applies to functions", name);
      *no_add_attrs = true;
    }
  else if (!VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
    {
      error ("%qE attribute requires a void return type", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "shared" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
nvptx_handle_shared_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != VAR_DECL)
    {
      error ("%qE attribute only applies to variables", name);
      *no_add_attrs = true;
    }
  else if (!(TREE_PUBLIC (decl) || TREE_STATIC (decl)))
    {
      error ("%qE attribute not allowed with auto storage class", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Table of valid machine attributes.  */
static const struct attribute_spec nvptx_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "kernel", 0, 0, true, false,  false, nvptx_handle_kernel_attribute, false },
  { "shared", 0, 0, true, false,  false, nvptx_handle_shared_attribute, false },
  { NULL, 0, 0, false, false, false, NULL, false }
};

/* Limit vector alignments to BIGGEST_ALIGNMENT.  */

static HOST_WIDE_INT
nvptx_vector_alignment (const_tree type)
{
  HOST_WIDE_INT align = tree_to_shwi (TYPE_SIZE (type));

  return MIN (align, BIGGEST_ALIGNMENT);
}

/* Indicate that INSN cannot be duplicated.   */

static bool
nvptx_cannot_copy_insn_p (rtx_insn *insn)
{
  switch (recog_memoized (insn))
    {
    case CODE_FOR_nvptx_shufflesi:
    case CODE_FOR_nvptx_shufflesf:
    case CODE_FOR_nvptx_barsync:
    case CODE_FOR_nvptx_fork:
    case CODE_FOR_nvptx_forked:
    case CODE_FOR_nvptx_joining:
    case CODE_FOR_nvptx_join:
      return true;
    default:
      return false;
    }
}

/* Section anchors do not work.  Initialization for flag_section_anchor
   probes the existence of the anchoring target hooks and prevents
   anchoring if they don't exist.  However, we may be being used with
   a host-side compiler that does support anchoring, and hence see
   the anchor flag set (as it's not recalculated).  So provide an
   implementation denying anchoring.  */

static bool
nvptx_use_anchors_for_symbol_p (const_rtx ARG_UNUSED (a))
{
  return false;
}

/* Record a symbol for mkoffload to enter into the mapping table.  */

static void
nvptx_record_offload_symbol (tree decl)
{
  switch (TREE_CODE (decl))
    {
    case VAR_DECL:
      fprintf (asm_out_file, "//:VAR_MAP \"%s\"\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
      break;

    case FUNCTION_DECL:
      {
	tree attr = oacc_get_fn_attrib (decl);
	/* OpenMP offloading does not set this attribute.  */
	tree dims = attr ? TREE_VALUE (attr) : NULL_TREE;

	fprintf (asm_out_file, "//:FUNC_MAP \"%s\"",
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));

	for (; dims; dims = TREE_CHAIN (dims))
	  {
	    int size = TREE_INT_CST_LOW (TREE_VALUE (dims));

	    gcc_assert (!TREE_PURPOSE (dims));
	    fprintf (asm_out_file, ", %#x", size);
	  }

	fprintf (asm_out_file, "\n");
      }
      break;

    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_ASM_FILE_START.  Write the kinds of things ptxas expects
   at the start of a file.  */

static void
nvptx_file_start (void)
{
  fputs ("// BEGIN PREAMBLE\n", asm_out_file);
  fputs ("\t.version\t3.1\n", asm_out_file);
  fputs ("\t.target\tsm_30\n", asm_out_file);
  fprintf (asm_out_file, "\t.address_size %d\n", GET_MODE_BITSIZE (Pmode));
  fputs ("// END PREAMBLE\n", asm_out_file);
}

/* Emit a declaration for a worker-level buffer in .shared memory.  */

static void
write_worker_buffer (FILE *file, rtx sym, unsigned align, unsigned size)
{
  const char *name = XSTR (sym, 0);

  write_var_marker (file, true, false, name);
  fprintf (file, ".shared .align %d .u8 %s[%d];\n",
	   align, name, size);
}

/* Write out the function declarations we've collected and declare storage
   for the broadcast buffer.  */

static void
nvptx_file_end (void)
{
  hash_table<tree_hasher>::iterator iter;
  tree decl;
  FOR_EACH_HASH_TABLE_ELEMENT (*needed_fndecls_htab, decl, tree, iter)
    nvptx_record_fndecl (decl);
  fputs (func_decls.str().c_str(), asm_out_file);

  if (worker_bcast_size)
    write_worker_buffer (asm_out_file, worker_bcast_sym,
			 worker_bcast_align, worker_bcast_size);

  if (worker_red_size)
    write_worker_buffer (asm_out_file, worker_red_sym,
			 worker_red_align, worker_red_size);

  if (need_softstack_decl)
    {
      write_var_marker (asm_out_file, false, true, "__nvptx_stacks");
      /* 32 is the maximum number of warps in a block.  Even though it's an
         external declaration, emit the array size explicitly; otherwise, it
         may fail at PTX JIT time if the definition is later in link order.  */
      fprintf (asm_out_file, ".extern .shared .u%d __nvptx_stacks[32];\n",
	       POINTER_SIZE);
    }
  if (need_unisimt_decl)
    {
      write_var_marker (asm_out_file, false, true, "__nvptx_uni");
      fprintf (asm_out_file, ".extern .shared .u32 __nvptx_uni[32];\n");
    }
}

/* Expander for the shuffle builtins.  */

static rtx
nvptx_expand_shuffle (tree exp, rtx target, machine_mode mode, int ignore)
{
  if (ignore)
    return target;
  
  rtx src = expand_expr (CALL_EXPR_ARG (exp, 0),
			 NULL_RTX, mode, EXPAND_NORMAL);
  if (!REG_P (src))
    src = copy_to_mode_reg (mode, src);

  rtx idx = expand_expr (CALL_EXPR_ARG (exp, 1),
			 NULL_RTX, SImode, EXPAND_NORMAL);
  rtx op = expand_expr (CALL_EXPR_ARG  (exp, 2),
			NULL_RTX, SImode, EXPAND_NORMAL);
  
  if (!REG_P (idx) && GET_CODE (idx) != CONST_INT)
    idx = copy_to_mode_reg (SImode, idx);

  rtx pat = nvptx_gen_shuffle (target, src, idx,
			       (nvptx_shuffle_kind) INTVAL (op));
  if (pat)
    emit_insn (pat);

  return target;
}

/* Worker reduction address expander.  */

static rtx
nvptx_expand_worker_addr (tree exp, rtx target,
			  machine_mode ARG_UNUSED (mode), int ignore)
{
  if (ignore)
    return target;

  unsigned align = TREE_INT_CST_LOW (CALL_EXPR_ARG (exp, 2));
  if (align > worker_red_align)
    worker_red_align = align;

  unsigned offset = TREE_INT_CST_LOW (CALL_EXPR_ARG (exp, 0));
  unsigned size = TREE_INT_CST_LOW (CALL_EXPR_ARG (exp, 1));
  if (size + offset > worker_red_size)
    worker_red_size = size + offset;

  rtx addr = worker_red_sym;
  if (offset)
    {
      addr = gen_rtx_PLUS (Pmode, addr, GEN_INT (offset));
      addr = gen_rtx_CONST (Pmode, addr);
    }

  emit_move_insn (target, addr);

  return target;
}

/* Expand the CMP_SWAP PTX builtins.  We have our own versions that do
   not require taking the address of any object, other than the memory
   cell being operated on.  */

static rtx
nvptx_expand_cmp_swap (tree exp, rtx target,
		       machine_mode ARG_UNUSED (m), int ARG_UNUSED (ignore))
{
  machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  
  if (!target)
    target = gen_reg_rtx (mode);

  rtx mem = expand_expr (CALL_EXPR_ARG (exp, 0),
			 NULL_RTX, Pmode, EXPAND_NORMAL);
  rtx cmp = expand_expr (CALL_EXPR_ARG (exp, 1),
			 NULL_RTX, mode, EXPAND_NORMAL);
  rtx src = expand_expr (CALL_EXPR_ARG (exp, 2),
			 NULL_RTX, mode, EXPAND_NORMAL);
  rtx pat;

  mem = gen_rtx_MEM (mode, mem);
  if (!REG_P (cmp))
    cmp = copy_to_mode_reg (mode, cmp);
  if (!REG_P (src))
    src = copy_to_mode_reg (mode, src);
  
  if (mode == SImode)
    pat = gen_atomic_compare_and_swapsi_1 (target, mem, cmp, src, const0_rtx);
  else
    pat = gen_atomic_compare_and_swapdi_1 (target, mem, cmp, src, const0_rtx);

  emit_insn (pat);

  return target;
}


/* Codes for all the NVPTX builtins.  */
enum nvptx_builtins
{
  NVPTX_BUILTIN_SHUFFLE,
  NVPTX_BUILTIN_SHUFFLELL,
  NVPTX_BUILTIN_WORKER_ADDR,
  NVPTX_BUILTIN_CMP_SWAP,
  NVPTX_BUILTIN_CMP_SWAPLL,
  NVPTX_BUILTIN_MAX
};

static GTY(()) tree nvptx_builtin_decls[NVPTX_BUILTIN_MAX];

/* Return the NVPTX builtin for CODE.  */

static tree
nvptx_builtin_decl (unsigned code, bool ARG_UNUSED (initialize_p))
{
  if (code >= NVPTX_BUILTIN_MAX)
    return error_mark_node;

  return nvptx_builtin_decls[code];
}

/* Set up all builtin functions for this target.  */

static void
nvptx_init_builtins (void)
{
#define DEF(ID, NAME, T)						\
  (nvptx_builtin_decls[NVPTX_BUILTIN_ ## ID]				\
   = add_builtin_function ("__builtin_nvptx_" NAME,			\
			   build_function_type_list T,			\
			   NVPTX_BUILTIN_ ## ID, BUILT_IN_MD, NULL, NULL))
#define ST sizetype
#define UINT unsigned_type_node
#define LLUINT long_long_unsigned_type_node
#define PTRVOID ptr_type_node

  DEF (SHUFFLE, "shuffle", (UINT, UINT, UINT, UINT, NULL_TREE));
  DEF (SHUFFLELL, "shufflell", (LLUINT, LLUINT, UINT, UINT, NULL_TREE));
  DEF (WORKER_ADDR, "worker_addr",
       (PTRVOID, ST, UINT, UINT, NULL_TREE));
  DEF (CMP_SWAP, "cmp_swap", (UINT, PTRVOID, UINT, UINT, NULL_TREE));
  DEF (CMP_SWAPLL, "cmp_swapll", (LLUINT, PTRVOID, LLUINT, LLUINT, NULL_TREE));

#undef DEF
#undef ST
#undef UINT
#undef LLUINT
#undef PTRVOID
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
nvptx_expand_builtin (tree exp, rtx target, rtx ARG_UNUSED (subtarget),
		      machine_mode mode, int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case NVPTX_BUILTIN_SHUFFLE:
    case NVPTX_BUILTIN_SHUFFLELL:
      return nvptx_expand_shuffle (exp, target, mode, ignore);

    case NVPTX_BUILTIN_WORKER_ADDR:
      return nvptx_expand_worker_addr (exp, target, mode, ignore);

    case NVPTX_BUILTIN_CMP_SWAP:
    case NVPTX_BUILTIN_CMP_SWAPLL:
      return nvptx_expand_cmp_swap (exp, target, mode, ignore);

    default: gcc_unreachable ();
    }
}

/* Define dimension sizes for known hardware.  */
#define PTX_VECTOR_LENGTH 32
#define PTX_WORKER_LENGTH 32
#define PTX_GANG_DEFAULT  0 /* Defer to runtime.  */

/* Implement TARGET_SIMT_VF target hook: number of threads in a warp.  */

static int
nvptx_simt_vf ()
{
  return PTX_VECTOR_LENGTH;
}

/* Validate compute dimensions of an OpenACC offload or routine, fill
   in non-unity defaults.  FN_LEVEL indicates the level at which a
   routine might spawn a loop.  It is negative for non-routines.  If
   DECL is null, we are validating the default dimensions.  */

static bool
nvptx_goacc_validate_dims (tree decl, int dims[], int fn_level)
{
  bool changed = false;

  /* The vector size must be 32, unless this is a SEQ routine.  */
  if (fn_level <= GOMP_DIM_VECTOR && fn_level >= -1
      && dims[GOMP_DIM_VECTOR] >= 0
      && dims[GOMP_DIM_VECTOR] != PTX_VECTOR_LENGTH)
    {
      if (fn_level < 0 && dims[GOMP_DIM_VECTOR] >= 0)
	warning_at (decl ? DECL_SOURCE_LOCATION (decl) : UNKNOWN_LOCATION, 0,
		    dims[GOMP_DIM_VECTOR]
		    ? G_("using vector_length (%d), ignoring %d")
		    : G_("using vector_length (%d), ignoring runtime setting"),
		    PTX_VECTOR_LENGTH, dims[GOMP_DIM_VECTOR]);
      dims[GOMP_DIM_VECTOR] = PTX_VECTOR_LENGTH;
      changed = true;
    }

  /* Check the num workers is not too large.  */
  if (dims[GOMP_DIM_WORKER] > PTX_WORKER_LENGTH)
    {
      warning_at (decl ? DECL_SOURCE_LOCATION (decl) : UNKNOWN_LOCATION, 0,
		  "using num_workers (%d), ignoring %d",
		  PTX_WORKER_LENGTH, dims[GOMP_DIM_WORKER]);
      dims[GOMP_DIM_WORKER] = PTX_WORKER_LENGTH;
      changed = true;
    }

  if (!decl)
    {
      dims[GOMP_DIM_VECTOR] = PTX_VECTOR_LENGTH;
      if (dims[GOMP_DIM_WORKER] < 0)
	dims[GOMP_DIM_WORKER] = PTX_WORKER_LENGTH;
      if (dims[GOMP_DIM_GANG] < 0)
	dims[GOMP_DIM_GANG] = PTX_GANG_DEFAULT;
      changed = true;
    }

  return changed;
}

/* Return maximum dimension size, or zero for unbounded.  */

static int
nvptx_dim_limit (int axis)
{
  switch (axis)
    {
    case GOMP_DIM_WORKER:
      return PTX_WORKER_LENGTH;

    case GOMP_DIM_VECTOR:
      return PTX_VECTOR_LENGTH;

    default:
      break;
    }
  return 0;
}

/* Determine whether fork & joins are needed.  */

static bool
nvptx_goacc_fork_join (gcall *call, const int dims[],
		       bool ARG_UNUSED (is_fork))
{
  tree arg = gimple_call_arg (call, 2);
  unsigned axis = TREE_INT_CST_LOW (arg);

  /* We only care about worker and vector partitioning.  */
  if (axis < GOMP_DIM_WORKER)
    return false;

  /* If the size is 1, there's no partitioning.  */
  if (dims[axis] == 1)
    return false;

  return true;
}

/* Generate a PTX builtin function call that returns the address in
   the worker reduction buffer at OFFSET.  TYPE is the type of the
   data at that location.  */

static tree
nvptx_get_worker_red_addr (tree type, tree offset)
{
  machine_mode mode = TYPE_MODE (type);
  tree fndecl = nvptx_builtin_decl (NVPTX_BUILTIN_WORKER_ADDR, true);
  tree size = build_int_cst (unsigned_type_node, GET_MODE_SIZE (mode));
  tree align = build_int_cst (unsigned_type_node,
			      GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT);
  tree call = build_call_expr (fndecl, 3, offset, size, align);

  return fold_convert (build_pointer_type (type), call);
}

/* Emit a SHFL.DOWN using index SHFL of VAR into DEST_VAR.  This function
   will cast the variable if necessary.  */

static void
nvptx_generate_vector_shuffle (location_t loc,
			       tree dest_var, tree var, unsigned shift,
			       gimple_seq *seq)
{
  unsigned fn = NVPTX_BUILTIN_SHUFFLE;
  tree_code code = NOP_EXPR;
  tree arg_type = unsigned_type_node;
  tree var_type = TREE_TYPE (var);
  tree dest_type = var_type;

  if (TREE_CODE (var_type) == COMPLEX_TYPE)
    var_type = TREE_TYPE (var_type);

  if (TREE_CODE (var_type) == REAL_TYPE)
    code = VIEW_CONVERT_EXPR;

  if (TYPE_SIZE (var_type)
      == TYPE_SIZE (long_long_unsigned_type_node))
    {
      fn = NVPTX_BUILTIN_SHUFFLELL;
      arg_type = long_long_unsigned_type_node;
    }
  
  tree call = nvptx_builtin_decl (fn, true);
  tree bits = build_int_cst (unsigned_type_node, shift);
  tree kind = build_int_cst (unsigned_type_node, SHUFFLE_DOWN);
  tree expr;

  if (var_type != dest_type)
    {
      /* Do real and imaginary parts separately.  */
      tree real = fold_build1 (REALPART_EXPR, var_type, var);
      real = fold_build1 (code, arg_type, real);
      real = build_call_expr_loc (loc, call, 3, real, bits, kind);
      real = fold_build1 (code, var_type, real);

      tree imag = fold_build1 (IMAGPART_EXPR, var_type, var);
      imag = fold_build1 (code, arg_type, imag);
      imag = build_call_expr_loc (loc, call, 3, imag, bits, kind);
      imag = fold_build1 (code, var_type, imag);

      expr = fold_build2 (COMPLEX_EXPR, dest_type, real, imag);
    }
  else
    {
      expr = fold_build1 (code, arg_type, var);
      expr = build_call_expr_loc (loc, call, 3, expr, bits, kind);
      expr = fold_build1 (code, dest_type, expr);
    }

  gimplify_assign (dest_var, expr, seq);
}

/* Lazily generate the global lock var decl and return its address.  */

static tree
nvptx_global_lock_addr ()
{
  tree v = global_lock_var;
  
  if (!v)
    {
      tree name = get_identifier ("__reduction_lock");
      tree type = build_qualified_type (unsigned_type_node,
					TYPE_QUAL_VOLATILE);
      v = build_decl (BUILTINS_LOCATION, VAR_DECL, name, type);
      global_lock_var = v;
      DECL_ARTIFICIAL (v) = 1;
      DECL_EXTERNAL (v) = 1;
      TREE_STATIC (v) = 1;
      TREE_PUBLIC (v) = 1;
      TREE_USED (v) = 1;
      mark_addressable (v);
      mark_decl_referenced (v);
    }

  return build_fold_addr_expr (v);
}

/* Insert code to locklessly update *PTR with *PTR OP VAR just before
   GSI.  We use a lockless scheme for nearly all case, which looks
   like:
     actual = initval(OP);
     do {
       guess = actual;
       write = guess OP myval;
       actual = cmp&swap (ptr, guess, write)
     } while (actual bit-different-to guess);
   return write;

   This relies on a cmp&swap instruction, which is available for 32-
   and 64-bit types.  Larger types must use a locking scheme.  */

static tree
nvptx_lockless_update (location_t loc, gimple_stmt_iterator *gsi,
		       tree ptr, tree var, tree_code op)
{
  unsigned fn = NVPTX_BUILTIN_CMP_SWAP;
  tree_code code = NOP_EXPR;
  tree arg_type = unsigned_type_node;
  tree var_type = TREE_TYPE (var);

  if (TREE_CODE (var_type) == COMPLEX_TYPE
      || TREE_CODE (var_type) == REAL_TYPE)
    code = VIEW_CONVERT_EXPR;

  if (TYPE_SIZE (var_type) == TYPE_SIZE (long_long_unsigned_type_node))
    {
      arg_type = long_long_unsigned_type_node;
      fn = NVPTX_BUILTIN_CMP_SWAPLL;
    }

  tree swap_fn = nvptx_builtin_decl (fn, true);

  gimple_seq init_seq = NULL;
  tree init_var = make_ssa_name (arg_type);
  tree init_expr = omp_reduction_init_op (loc, op, var_type);
  init_expr = fold_build1 (code, arg_type, init_expr);
  gimplify_assign (init_var, init_expr, &init_seq);
  gimple *init_end = gimple_seq_last (init_seq);

  gsi_insert_seq_before (gsi, init_seq, GSI_SAME_STMT);
  
  /* Split the block just after the init stmts.  */
  basic_block pre_bb = gsi_bb (*gsi);
  edge pre_edge = split_block (pre_bb, init_end);
  basic_block loop_bb = pre_edge->dest;
  pre_bb = pre_edge->src;
  /* Reset the iterator.  */
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  tree expect_var = make_ssa_name (arg_type);
  tree actual_var = make_ssa_name (arg_type);
  tree write_var = make_ssa_name (arg_type);
  
  /* Build and insert the reduction calculation.  */
  gimple_seq red_seq = NULL;
  tree write_expr = fold_build1 (code, var_type, expect_var);
  write_expr = fold_build2 (op, var_type, write_expr, var);
  write_expr = fold_build1 (code, arg_type, write_expr);
  gimplify_assign (write_var, write_expr, &red_seq);

  gsi_insert_seq_before (gsi, red_seq, GSI_SAME_STMT);

  /* Build & insert the cmp&swap sequence.  */
  gimple_seq latch_seq = NULL;
  tree swap_expr = build_call_expr_loc (loc, swap_fn, 3,
					ptr, expect_var, write_var);
  gimplify_assign (actual_var, swap_expr, &latch_seq);

  gcond *cond = gimple_build_cond (EQ_EXPR, actual_var, expect_var,
				   NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&latch_seq, cond);

  gimple *latch_end = gimple_seq_last (latch_seq);
  gsi_insert_seq_before (gsi, latch_seq, GSI_SAME_STMT);

  /* Split the block just after the latch stmts.  */
  edge post_edge = split_block (loop_bb, latch_end);
  basic_block post_bb = post_edge->dest;
  loop_bb = post_edge->src;
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  post_edge->flags ^= EDGE_TRUE_VALUE | EDGE_FALLTHRU;
  post_edge->probability = profile_probability::even ();
  edge loop_edge = make_edge (loop_bb, loop_bb, EDGE_FALSE_VALUE);
  loop_edge->probability = profile_probability::even ();
  set_immediate_dominator (CDI_DOMINATORS, loop_bb, pre_bb);
  set_immediate_dominator (CDI_DOMINATORS, post_bb, loop_bb);

  gphi *phi = create_phi_node (expect_var, loop_bb);
  add_phi_arg (phi, init_var, pre_edge, loc);
  add_phi_arg (phi, actual_var, loop_edge, loc);

  loop *loop = alloc_loop ();
  loop->header = loop_bb;
  loop->latch = loop_bb;
  add_loop (loop, loop_bb->loop_father);

  return fold_build1 (code, var_type, write_var);
}

/* Insert code to lockfully update *PTR with *PTR OP VAR just before
   GSI.  This is necessary for types larger than 64 bits, where there
   is no cmp&swap instruction to implement a lockless scheme.  We use
   a lock variable in global memory.

   while (cmp&swap (&lock_var, 0, 1))
     continue;
   T accum = *ptr;
   accum = accum OP var;
   *ptr = accum;
   cmp&swap (&lock_var, 1, 0);
   return accum;

   A lock in global memory is necessary to force execution engine
   descheduling and avoid resource starvation that can occur if the
   lock is in .shared memory.  */

static tree
nvptx_lockfull_update (location_t loc, gimple_stmt_iterator *gsi,
		       tree ptr, tree var, tree_code op)
{
  tree var_type = TREE_TYPE (var);
  tree swap_fn = nvptx_builtin_decl (NVPTX_BUILTIN_CMP_SWAP, true);
  tree uns_unlocked = build_int_cst (unsigned_type_node, 0);
  tree uns_locked = build_int_cst (unsigned_type_node, 1);

  /* Split the block just before the gsi.  Insert a gimple nop to make
     this easier.  */
  gimple *nop = gimple_build_nop ();
  gsi_insert_before (gsi, nop, GSI_SAME_STMT);
  basic_block entry_bb = gsi_bb (*gsi);
  edge entry_edge = split_block (entry_bb, nop);
  basic_block lock_bb = entry_edge->dest;
  /* Reset the iterator.  */
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));

  /* Build and insert the locking sequence.  */
  gimple_seq lock_seq = NULL;
  tree lock_var = make_ssa_name (unsigned_type_node);
  tree lock_expr = nvptx_global_lock_addr ();
  lock_expr = build_call_expr_loc (loc, swap_fn, 3, lock_expr,
				   uns_unlocked, uns_locked);
  gimplify_assign (lock_var, lock_expr, &lock_seq);
  gcond *cond = gimple_build_cond (EQ_EXPR, lock_var, uns_unlocked,
				   NULL_TREE, NULL_TREE);
  gimple_seq_add_stmt (&lock_seq, cond);
  gimple *lock_end = gimple_seq_last (lock_seq);
  gsi_insert_seq_before (gsi, lock_seq, GSI_SAME_STMT);

  /* Split the block just after the lock sequence.  */
  edge locked_edge = split_block (lock_bb, lock_end);
  basic_block update_bb = locked_edge->dest;
  lock_bb = locked_edge->src;
  *gsi = gsi_for_stmt (gsi_stmt (*gsi));
  
  /* Create the lock loop ... */
  locked_edge->flags ^= EDGE_TRUE_VALUE | EDGE_FALLTHRU;
  locked_edge->probability = profile_probability::even ();
  edge loop_edge = make_edge (lock_bb, lock_bb, EDGE_FALSE_VALUE);
  loop_edge->probability = profile_probability::even ();
  set_immediate_dominator (CDI_DOMINATORS, lock_bb, entry_bb);
  set_immediate_dominator (CDI_DOMINATORS, update_bb, lock_bb);

  /* ... and the loop structure.  */
  loop *lock_loop = alloc_loop ();
  lock_loop->header = lock_bb;
  lock_loop->latch = lock_bb;
  lock_loop->nb_iterations_estimate = 1;
  lock_loop->any_estimate = true;
  add_loop (lock_loop, entry_bb->loop_father);

  /* Build and insert the reduction calculation.  */
  gimple_seq red_seq = NULL;
  tree acc_in = make_ssa_name (var_type);
  tree ref_in = build_simple_mem_ref (ptr);
  TREE_THIS_VOLATILE (ref_in) = 1;
  gimplify_assign (acc_in, ref_in, &red_seq);
  
  tree acc_out = make_ssa_name (var_type);
  tree update_expr = fold_build2 (op, var_type, ref_in, var);
  gimplify_assign (acc_out, update_expr, &red_seq);
  
  tree ref_out = build_simple_mem_ref (ptr);
  TREE_THIS_VOLATILE (ref_out) = 1;
  gimplify_assign (ref_out, acc_out, &red_seq);

  gsi_insert_seq_before (gsi, red_seq, GSI_SAME_STMT);

  /* Build & insert the unlock sequence.  */
  gimple_seq unlock_seq = NULL;
  tree unlock_expr = nvptx_global_lock_addr ();
  unlock_expr = build_call_expr_loc (loc, swap_fn, 3, unlock_expr,
				     uns_locked, uns_unlocked);
  gimplify_and_add (unlock_expr, &unlock_seq);
  gsi_insert_seq_before (gsi, unlock_seq, GSI_SAME_STMT);

  return acc_out;
}

/* Emit a sequence to update a reduction accumlator at *PTR with the
   value held in VAR using operator OP.  Return the updated value.

   TODO: optimize for atomic ops and indepedent complex ops.  */

static tree
nvptx_reduction_update (location_t loc, gimple_stmt_iterator *gsi,
			tree ptr, tree var, tree_code op)
{
  tree type = TREE_TYPE (var);
  tree size = TYPE_SIZE (type);

  if (size == TYPE_SIZE (unsigned_type_node)
      || size == TYPE_SIZE (long_long_unsigned_type_node))
    return nvptx_lockless_update (loc, gsi, ptr, var, op);
  else
    return nvptx_lockfull_update (loc, gsi, ptr, var, op);
}

/* NVPTX implementation of GOACC_REDUCTION_SETUP.  */

static void
nvptx_goacc_reduction_setup (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  gimple_seq seq = NULL;

  push_gimplify_context (true);

  if (level != GOMP_DIM_GANG)
    {
      /* Copy the receiver object.  */
      tree ref_to_res = gimple_call_arg (call, 1);

      if (!integer_zerop (ref_to_res))
	var = build_simple_mem_ref (ref_to_res);
    }
  
  if (level == GOMP_DIM_WORKER)
    {
      /* Store incoming value to worker reduction buffer.  */
      tree offset = gimple_call_arg (call, 5);
      tree call = nvptx_get_worker_red_addr (TREE_TYPE (var), offset);
      tree ptr = make_ssa_name (TREE_TYPE (call));

      gimplify_assign (ptr, call, &seq);
      tree ref = build_simple_mem_ref (ptr);
      TREE_THIS_VOLATILE (ref) = 1;
      gimplify_assign (ref, var, &seq);
    }

  if (lhs)
    gimplify_assign (lhs, var, &seq);

  pop_gimplify_context (NULL);
  gsi_replace_with_seq (&gsi, seq, true);
}

/* NVPTX implementation of GOACC_REDUCTION_INIT. */

static void
nvptx_goacc_reduction_init (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  enum tree_code rcode
    = (enum tree_code)TREE_INT_CST_LOW (gimple_call_arg (call, 4));
  tree init = omp_reduction_init_op (gimple_location (call), rcode,
				     TREE_TYPE (var));
  gimple_seq seq = NULL;
  
  push_gimplify_context (true);

  if (level == GOMP_DIM_VECTOR)
    {
      /* Initialize vector-non-zeroes to INIT_VAL (OP).  */
      tree tid = make_ssa_name (integer_type_node);
      tree dim_vector = gimple_call_arg (call, 3);
      gimple *tid_call = gimple_build_call_internal (IFN_GOACC_DIM_POS, 1,
						     dim_vector);
      gimple *cond_stmt = gimple_build_cond (NE_EXPR, tid, integer_zero_node,
					     NULL_TREE, NULL_TREE);

      gimple_call_set_lhs (tid_call, tid);
      gimple_seq_add_stmt (&seq, tid_call);
      gimple_seq_add_stmt (&seq, cond_stmt);

      /* Split the block just after the call.  */
      edge init_edge = split_block (gsi_bb (gsi), call);
      basic_block init_bb = init_edge->dest;
      basic_block call_bb = init_edge->src;

      /* Fixup flags from call_bb to init_bb.  */
      init_edge->flags ^= EDGE_FALLTHRU | EDGE_TRUE_VALUE;
      init_edge->probability = profile_probability::even ();
      
      /* Set the initialization stmts.  */
      gimple_seq init_seq = NULL;
      tree init_var = make_ssa_name (TREE_TYPE (var));
      gimplify_assign (init_var, init, &init_seq);
      gsi = gsi_start_bb (init_bb);
      gsi_insert_seq_before (&gsi, init_seq, GSI_SAME_STMT);

      /* Split block just after the init stmt.  */
      gsi_prev (&gsi);
      edge inited_edge = split_block (gsi_bb (gsi), gsi_stmt (gsi));
      basic_block dst_bb = inited_edge->dest;
      
      /* Create false edge from call_bb to dst_bb.  */
      edge nop_edge = make_edge (call_bb, dst_bb, EDGE_FALSE_VALUE);
      nop_edge->probability = profile_probability::even ();

      /* Create phi node in dst block.  */
      gphi *phi = create_phi_node (lhs, dst_bb);
      add_phi_arg (phi, init_var, inited_edge, gimple_location (call));
      add_phi_arg (phi, var, nop_edge, gimple_location (call));

      /* Reset dominator of dst bb.  */
      set_immediate_dominator (CDI_DOMINATORS, dst_bb, call_bb);

      /* Reset the gsi.  */
      gsi = gsi_for_stmt (call);
    }
  else
    {
      if (level == GOMP_DIM_GANG)
	{
	  /* If there's no receiver object, propagate the incoming VAR.  */
	  tree ref_to_res = gimple_call_arg (call, 1);
	  if (integer_zerop (ref_to_res))
	    init = var;
	}

      gimplify_assign (lhs, init, &seq);
    }

  pop_gimplify_context (NULL);
  gsi_replace_with_seq (&gsi, seq, true);
}

/* NVPTX implementation of GOACC_REDUCTION_FINI.  */

static void
nvptx_goacc_reduction_fini (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree ref_to_res = gimple_call_arg (call, 1);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  enum tree_code op
    = (enum tree_code)TREE_INT_CST_LOW (gimple_call_arg (call, 4));
  gimple_seq seq = NULL;
  tree r = NULL_TREE;;

  push_gimplify_context (true);

  if (level == GOMP_DIM_VECTOR)
    {
      /* Emit binary shuffle tree.  TODO. Emit this as an actual loop,
	 but that requires a method of emitting a unified jump at the
	 gimple level.  */
      for (int shfl = PTX_VECTOR_LENGTH / 2; shfl > 0; shfl = shfl >> 1)
	{
	  tree other_var = make_ssa_name (TREE_TYPE (var));
	  nvptx_generate_vector_shuffle (gimple_location (call),
					 other_var, var, shfl, &seq);

	  r = make_ssa_name (TREE_TYPE (var));
	  gimplify_assign (r, fold_build2 (op, TREE_TYPE (var),
					   var, other_var), &seq);
	  var = r;
	}
    }
  else
    {
      tree accum = NULL_TREE;

      if (level == GOMP_DIM_WORKER)
	{
	  /* Get reduction buffer address.  */
	  tree offset = gimple_call_arg (call, 5);
	  tree call = nvptx_get_worker_red_addr (TREE_TYPE (var), offset);
	  tree ptr = make_ssa_name (TREE_TYPE (call));

	  gimplify_assign (ptr, call, &seq);
	  accum = ptr;
	}
      else if (integer_zerop (ref_to_res))
	r = var;
      else
	accum = ref_to_res;

      if (accum)
	{
	  /* UPDATE the accumulator.  */
	  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
	  seq = NULL;
	  r = nvptx_reduction_update (gimple_location (call), &gsi,
				      accum, var, op);
	}
    }

  if (lhs)
    gimplify_assign (lhs, r, &seq);
  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* NVPTX implementation of GOACC_REDUCTION_TEARDOWN.  */

static void
nvptx_goacc_reduction_teardown (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
  gimple_seq seq = NULL;
  
  push_gimplify_context (true);
  if (level == GOMP_DIM_WORKER)
    {
      /* Read the worker reduction buffer.  */
      tree offset = gimple_call_arg (call, 5);
      tree call = nvptx_get_worker_red_addr(TREE_TYPE (var), offset);
      tree ptr = make_ssa_name (TREE_TYPE (call));

      gimplify_assign (ptr, call, &seq);
      var = build_simple_mem_ref (ptr);
      TREE_THIS_VOLATILE (var) = 1;
    }

  if (level != GOMP_DIM_GANG)
    {
      /* Write to the receiver object.  */
      tree ref_to_res = gimple_call_arg (call, 1);

      if (!integer_zerop (ref_to_res))
	gimplify_assign (build_simple_mem_ref (ref_to_res), var, &seq);
    }

  if (lhs)
    gimplify_assign (lhs, var, &seq);
  
  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* NVPTX reduction expander.  */

static void
nvptx_goacc_reduction (gcall *call)
{
  unsigned code = (unsigned)TREE_INT_CST_LOW (gimple_call_arg (call, 0));

  switch (code)
    {
    case IFN_GOACC_REDUCTION_SETUP:
      nvptx_goacc_reduction_setup (call);
      break;

    case IFN_GOACC_REDUCTION_INIT:
      nvptx_goacc_reduction_init (call);
      break;

    case IFN_GOACC_REDUCTION_FINI:
      nvptx_goacc_reduction_fini (call);
      break;

    case IFN_GOACC_REDUCTION_TEARDOWN:
      nvptx_goacc_reduction_teardown (call);
      break;

    default:
      gcc_unreachable ();
    }
}

static bool
nvptx_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED,
			      rtx x ATTRIBUTE_UNUSED)
{
  return true;
}

static bool
nvptx_vector_mode_supported (machine_mode mode)
{
  return (mode == V2SImode
	  || mode == V2DImode);
}

/* Return the preferred mode for vectorizing scalar MODE.  */

static machine_mode
nvptx_preferred_simd_mode (machine_mode mode)
{
  switch (mode)
    {
    case DImode:
      return V2DImode;
    case SImode:
      return V2SImode;

    default:
      return default_preferred_simd_mode (mode);
    }
}

unsigned int
nvptx_data_alignment (const_tree type, unsigned int basic_align)
{
  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      unsigned HOST_WIDE_INT size = tree_to_uhwi (TYPE_SIZE_UNIT (type));
      if (size == GET_MODE_SIZE (TImode))
	return GET_MODE_BITSIZE (maybe_split_mode (TImode));
    }

  return basic_align;
}

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nvptx_option_override

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE nvptx_attribute_table

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P nvptx_legitimate_address_p

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE nvptx_promote_function_mode

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nvptx_function_arg
#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG nvptx_function_incoming_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nvptx_function_arg_advance
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY nvptx_function_arg_boundary
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE nvptx_pass_by_reference
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nvptx_function_value_regno_p
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nvptx_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nvptx_libcall_value
#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL nvptx_function_ok_for_sibcall
#undef TARGET_GET_DRAP_RTX
#define TARGET_GET_DRAP_RTX nvptx_get_drap_rtx
#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG hook_bool_const_tree_true
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nvptx_return_in_memory
#undef TARGET_OMIT_STRUCT_RETURN_REG
#define TARGET_OMIT_STRUCT_RETURN_REG true
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING nvptx_strict_argument_naming
#undef TARGET_CALL_ARGS
#define TARGET_CALL_ARGS nvptx_call_args
#undef TARGET_END_CALL_ARGS
#define TARGET_END_CALL_ARGS nvptx_end_call_args

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START nvptx_file_start
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END nvptx_file_end
#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL nvptx_globalize_label
#undef TARGET_ASM_ASSEMBLE_UNDEFINED_DECL
#define TARGET_ASM_ASSEMBLE_UNDEFINED_DECL nvptx_assemble_undefined_decl
#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nvptx_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nvptx_print_operand_address
#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P nvptx_print_operand_punct_valid_p
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER nvptx_assemble_integer
#undef TARGET_ASM_DECL_END
#define TARGET_ASM_DECL_END nvptx_assemble_decl_end
#undef TARGET_ASM_DECLARE_CONSTANT_NAME
#define TARGET_ASM_DECLARE_CONSTANT_NAME nvptx_asm_declare_constant_name
#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P hook_bool_mode_const_rtx_true
#undef TARGET_ASM_NEED_VAR_DECL_BEFORE_USE
#define TARGET_ASM_NEED_VAR_DECL_BEFORE_USE true

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG nvptx_reorg
#undef TARGET_NO_REGISTER_ALLOCATION
#define TARGET_NO_REGISTER_ALLOCATION true

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO nvptx_encode_section_info
#undef TARGET_RECORD_OFFLOAD_SYMBOL
#define TARGET_RECORD_OFFLOAD_SYMBOL nvptx_record_offload_symbol

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT nvptx_vector_alignment

#undef TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P nvptx_cannot_copy_insn_p

#undef TARGET_USE_ANCHORS_FOR_SYMBOL_P
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P nvptx_use_anchors_for_symbol_p

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS nvptx_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN nvptx_expand_builtin
#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL nvptx_builtin_decl

#undef TARGET_SIMT_VF
#define TARGET_SIMT_VF nvptx_simt_vf

#undef TARGET_GOACC_VALIDATE_DIMS
#define TARGET_GOACC_VALIDATE_DIMS nvptx_goacc_validate_dims

#undef TARGET_GOACC_DIM_LIMIT
#define TARGET_GOACC_DIM_LIMIT nvptx_dim_limit

#undef TARGET_GOACC_FORK_JOIN
#define TARGET_GOACC_FORK_JOIN nvptx_goacc_fork_join

#undef TARGET_GOACC_REDUCTION
#define TARGET_GOACC_REDUCTION nvptx_goacc_reduction

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM nvptx_cannot_force_const_mem

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P nvptx_vector_mode_supported

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE \
    nvptx_preferred_simd_mode

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-nvptx.h"
