/* Target code for NVPTX.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.
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
#include "tm.h"
#include "rtl.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "hashtab.h"
#include "hard-reg-set.h"
#include "function.h"
#include "flags.h"
#include "statistics.h"
#include "real.h"
#include "fixed-value.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "regs.h"
#include "optabs.h"
#include "recog.h"
#include "ggc.h"
#include "timevar.h"
#include "tm_p.h"
#include "tm-preds.h"
#include "tm-constrs.h"
#include "langhooks.h"
#include "dbxout.h"
#include "target.h"
#include "target-def.h"
#include "diagnostic.h"
#include "predict.h"
#include "basic-block.h"
#include "cfgrtl.h"
#include "stor-layout.h"
#include "df.h"
#include "builtins.h"

/* Record the function decls we've written, and the libfuncs and function
   decls corresponding to them.  */
static std::stringstream func_decls;

struct declared_libfunc_hasher : ggc_cache_hasher<rtx>
{
  static hashval_t hash (rtx x) { return htab_hash_pointer (x); }
  static bool equal (rtx a, rtx b) { return a == b; }
};

static GTY((cache))
  hash_table<declared_libfunc_hasher> *declared_libfuncs_htab;

  struct tree_hasher : ggc_cache_hasher<tree>
{
  static hashval_t hash (tree t) { return htab_hash_pointer (t); }
  static bool equal (tree a, tree b) { return a == b; }
};

static GTY((cache)) hash_table<tree_hasher> *declared_fndecls_htab;
static GTY((cache)) hash_table<tree_hasher> *needed_fndecls_htab;

/* Allocate a new, cleared machine_function structure.  */

static struct machine_function *
nvptx_init_machine_status (void)
{
  struct machine_function *p = ggc_cleared_alloc<machine_function> ();
  p->ret_reg_mode = VOIDmode;
  return p;
}

/* Implement TARGET_OPTION_OVERRIDE.  */

static void
nvptx_option_override (void)
{
  init_machine_status = nvptx_init_machine_status;
  /* Gives us a predictable order, which we need especially for variables.  */
  flag_toplevel_reorder = 1;
  /* Assumes that it will see only hard registers.  */
  flag_var_tracking = 0;
  write_symbols = NO_DEBUG;
  debug_info_level = DINFO_LEVEL_NONE;

  declared_fndecls_htab = hash_table<tree_hasher>::create_ggc (17);
  needed_fndecls_htab = hash_table<tree_hasher>::create_ggc (17);
  declared_libfuncs_htab
    = hash_table<declared_libfunc_hasher>::create_ggc (17);
}

/* Return the mode to be used when declaring a ptx object for OBJ.
   For objects with subparts such as complex modes this is the mode
   of the subpart.  */

machine_mode
nvptx_underlying_object_mode (rtx obj)
{
  if (GET_CODE (obj) == SUBREG)
    obj = SUBREG_REG (obj);
  machine_mode mode = GET_MODE (obj);
  if (mode == TImode)
    return DImode;
  if (COMPLEX_MODE_P (mode))
    return GET_MODE_INNER (mode);
  return mode;
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

    default:
      gcc_unreachable ();
    }
}

/* Return the number of pieces to use when dealing with a pseudo of *PMODE.
   Alter *PMODE if we return a number greater than one.  */

static int
maybe_split_mode (machine_mode *pmode)
{
  machine_mode mode = *pmode;

  if (COMPLEX_MODE_P (mode))
    {
      *pmode = GET_MODE_INNER (mode);
      return 2;
    }
  else if (mode == TImode)
    {
      *pmode = DImode;
      return 2;
    }
  return 1;
}

/* Like maybe_split_mode, but only return whether or not the mode
   needs to be split.  */
static bool
nvptx_split_reg_p (machine_mode mode)
{
  if (COMPLEX_MODE_P (mode))
    return true;
  if (mode == TImode)
    return true;
  return false;
}

#define PASS_IN_REG_P(MODE, TYPE)				\
  ((GET_MODE_CLASS (MODE) == MODE_INT				\
    || GET_MODE_CLASS (MODE) == MODE_FLOAT			\
    || ((GET_MODE_CLASS (MODE) == MODE_COMPLEX_INT		\
	 || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)	\
	&& !AGGREGATE_TYPE_P (TYPE)))				\
   && (MODE) != TImode)

#define RETURN_IN_REG_P(MODE)			\
  ((GET_MODE_CLASS (MODE) == MODE_INT		\
    || GET_MODE_CLASS (MODE) == MODE_FLOAT)	\
   && GET_MODE_SIZE (MODE) <= 8)

/* Perform a mode promotion for a function argument with MODE.  Return
   the promoted mode.  */

static machine_mode
arg_promotion (machine_mode mode)
{
  if (mode == QImode || mode == HImode)
    return SImode;
  return mode;
}

/* Write the declaration of a function arg of TYPE to S.  I is the index
   of the argument, MODE its mode.  NO_ARG_TYPES is true if this is for
   a decl with zero TYPE_ARG_TYPES, i.e. an old-style C decl.  */

static int
write_one_arg (std::stringstream &s, tree type, int i, machine_mode mode,
	       bool no_arg_types)
{
  if (!PASS_IN_REG_P (mode, type))
    mode = Pmode;

  int count = maybe_split_mode (&mode);

  if (count == 2)
    {
      write_one_arg (s, NULL_TREE, i, mode, false);
      write_one_arg (s, NULL_TREE, i + 1, mode, false);
      return i + 1;
    }

  if (no_arg_types && !AGGREGATE_TYPE_P (type))
    {
      if (mode == SFmode)
	mode = DFmode;
      mode = arg_promotion (mode);
    }

  if (i > 0)
    s << ", ";
  s << ".param" << nvptx_ptx_type_from_mode (mode, false) << " %in_ar"
    << (i + 1) << (mode == QImode || mode == HImode ? "[1]" : "");
  if (mode == BLKmode)
    s << "[" << int_size_in_bytes (type) << "]";
  return i;
}

/* Look for attributes in ATTRS that would indicate we must write a function
   as a .entry kernel rather than a .func.  Return true if one is found.  */

static bool
write_as_kernel (tree attrs)
{
  return (lookup_attribute ("kernel", attrs) != NULL_TREE
	  || lookup_attribute ("omp target entrypoint", attrs) != NULL_TREE);
}

/* Write a function decl for DECL to S, where NAME is the name to be used.  */

static void
nvptx_write_function_decl (std::stringstream &s, const char *name, const_tree decl)
{
  tree fntype = TREE_TYPE (decl);
  tree result_type = TREE_TYPE (fntype);
  tree args = TYPE_ARG_TYPES (fntype);
  tree attrs = DECL_ATTRIBUTES (decl);
  bool kernel = write_as_kernel (attrs);
  bool is_main = strcmp (name, "main") == 0;
  bool args_from_decl = false;

  /* We get:
     NULL in TYPE_ARG_TYPES, for old-style functions
     NULL in DECL_ARGUMENTS, for builtin functions without another
       declaration.
     So we have to pick the best one we have.  */
  if (args == 0)
    {
      args = DECL_ARGUMENTS (decl);
      args_from_decl = true;
    }

  if (DECL_EXTERNAL (decl))
    s << ".extern ";
  else if (TREE_PUBLIC (decl))
    s << ".visible ";

  if (kernel)
    s << ".entry ";
  else
    s << ".func ";

  /* Declare the result.  */
  bool return_in_mem = false;
  if (TYPE_MODE (result_type) != VOIDmode)
    {
      machine_mode mode = TYPE_MODE (result_type);
      if (!RETURN_IN_REG_P (mode))
	return_in_mem = true;
      else
	{
	  mode = arg_promotion (mode);
	  s << "(.param" << nvptx_ptx_type_from_mode (mode, false)
	    << " %out_retval)";
	}
    }

  if (name[0] == '*')
    s << (name + 1);
  else
    s << name;

  /* Declare argument types.  */
  if ((args != NULL_TREE
       && !(TREE_CODE (args) == TREE_LIST && TREE_VALUE (args) == void_type_node))
      || is_main
      || return_in_mem
      || DECL_STATIC_CHAIN (decl))
    {
      s << "(";
      int i = 0;
      bool any_args = false;
      if (return_in_mem)
	{
	  s << ".param.u" << GET_MODE_BITSIZE (Pmode) << " %in_ar1";
	  i++;
	}
      while (args != NULL_TREE)
	{
	  tree type = args_from_decl ? TREE_TYPE (args) : TREE_VALUE (args);
	  machine_mode mode = TYPE_MODE (type);

	  if (mode != VOIDmode)
	    {
	      i = write_one_arg (s, type, i, mode,
				 TYPE_ARG_TYPES (fntype) == 0);
	      any_args = true;
	      i++;
	    }
	  args = TREE_CHAIN (args);
	}
      if (stdarg_p (fntype))
	{
	  gcc_assert (i > 0);
	  s << ", .param.u" << GET_MODE_BITSIZE (Pmode) << " %in_argp";
	}
      if (DECL_STATIC_CHAIN (decl))
	{
	  if (i > 0)
	    s << ", ";
	  s << ".reg.u" << GET_MODE_BITSIZE (Pmode)
	    << reg_names [STATIC_CHAIN_REGNUM];
	}
      if (!any_args && is_main)
	s << ".param.u32 %argc, .param.u" << GET_MODE_BITSIZE (Pmode)
	  << " %argv";
      s << ")";
    }
}

/* Walk either ARGTYPES or ARGS if the former is null, and write out part of
   the function header to FILE.  If WRITE_COPY is false, write reg
   declarations, otherwise write the copy from the incoming argument to that
   reg.  RETURN_IN_MEM indicates whether to start counting arg numbers at 1
   instead of 0.  */

static void
walk_args_for_param (FILE *file, tree argtypes, tree args, bool write_copy,
		     bool return_in_mem)
{
  int i;

  bool args_from_decl = false;
  if (argtypes == 0)
    args_from_decl = true;
  else
    args = argtypes;

  for (i = return_in_mem ? 1 : 0; args != NULL_TREE; args = TREE_CHAIN (args))
    {
      tree type = args_from_decl ? TREE_TYPE (args) : TREE_VALUE (args);
      machine_mode mode = TYPE_MODE (type);

      if (mode == VOIDmode)
	break;

      if (!PASS_IN_REG_P (mode, type))
	mode = Pmode;

      int count = maybe_split_mode (&mode);
      if (count == 1)
	{
	  if (argtypes == NULL && !AGGREGATE_TYPE_P (type))
	    {
	      if (mode == SFmode)
		mode = DFmode;

	    }
	  mode = arg_promotion (mode);
	}
      while (count-- > 0)
	{
	  i++;
	  if (write_copy)
	    fprintf (file, "\tld.param%s %%ar%d, [%%in_ar%d];\n",
		     nvptx_ptx_type_from_mode (mode, false), i, i);
	  else
	    fprintf (file, "\t.reg%s %%ar%d;\n",
		     nvptx_ptx_type_from_mode (mode, false), i);
	}
    }
}

/* Write a .func or .kernel declaration (not a definition) along with
   a helper comment for use by ld.  S is the stream to write to, DECL
   the decl for the function with name NAME.  */

static void
write_function_decl_and_comment (std::stringstream &s, const char *name, const_tree decl)
{
  s << "// BEGIN";
  if (TREE_PUBLIC (decl))
    s << " GLOBAL";
  s << " FUNCTION DECL: ";
  if (name[0] == '*')
    s << (name + 1);
  else
    s << name;
  s << "\n";
  nvptx_write_function_decl (s, name, decl);
  s << ";\n";
}

/* Check NAME for special function names and redirect them by returning a
   replacement.  This applies to malloc, free and realloc, for which we
   want to use libgcc wrappers, and call, which triggers a bug in ptxas.  */

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

/* If DECL is a FUNCTION_DECL, check the hash table to see if we
   already encountered it, and if not, insert it and write a ptx
   declarations that will be output at the end of compilation.  */

static bool
nvptx_record_fndecl (tree decl, bool force = false)
{
  if (decl == NULL_TREE || TREE_CODE (decl) != FUNCTION_DECL
      || !DECL_EXTERNAL (decl))
    return true;

  if (!force && TYPE_ARG_TYPES (TREE_TYPE (decl)) == NULL_TREE)
    return false;

  tree *slot = declared_fndecls_htab->find_slot (decl, INSERT);
  if (*slot == NULL)
    {
      *slot = decl;
      const char *name = get_fnname_from_decl (decl);
      name = nvptx_name_replacement (name);
      write_function_decl_and_comment (func_decls, name, decl);
    }
  return true;
}

/* Record that we need to emit a ptx decl for DECL.  Either do it now, or
   record it for later in case we have no argument information at this
   point.  */

void
nvptx_record_needed_fndecl (tree decl)
{
  if (nvptx_record_fndecl (decl))
    return;

  tree *slot = needed_fndecls_htab->find_slot (decl, INSERT);
  if (*slot == NULL)
    *slot = decl;
}

/* Implement ASM_DECLARE_FUNCTION_NAME.  Writes the start of a ptx
   function, including local var decls and copies from the arguments to
   local regs.  */

void
nvptx_declare_function_name (FILE *file, const char *name, const_tree decl)
{
  tree fntype = TREE_TYPE (decl);
  tree result_type = TREE_TYPE (fntype);

  name = nvptx_name_replacement (name);

  std::stringstream s;
  write_function_decl_and_comment (s, name, decl);
  s << "// BEGIN";
  if (TREE_PUBLIC (decl))
    s << " GLOBAL";
  s << " FUNCTION DEF: ";

  if (name[0] == '*')
    s << (name + 1);
  else
    s << name;
  s << "\n";

  nvptx_write_function_decl (s, name, decl);
  fprintf (file, "%s", s.str().c_str());

  bool return_in_mem = false;
  if (TYPE_MODE (result_type) != VOIDmode)
    {
      machine_mode mode = TYPE_MODE (result_type);
      if (!RETURN_IN_REG_P (mode))
	return_in_mem = true;
    }

  fprintf (file, "\n{\n");

  /* Ensure all arguments that should live in a register have one
     declared.  We'll emit the copies below.  */
  walk_args_for_param (file, TYPE_ARG_TYPES (fntype), DECL_ARGUMENTS (decl),
		       false, return_in_mem);
  if (return_in_mem)
    fprintf (file, "\t.reg.u%d %%ar1;\n", GET_MODE_BITSIZE (Pmode));
  else if (TYPE_MODE (result_type) != VOIDmode)
    {
      machine_mode mode = arg_promotion (TYPE_MODE (result_type));
      fprintf (file, ".reg%s %%retval;\n",
	       nvptx_ptx_type_from_mode (mode, false));
    }

  if (stdarg_p (fntype))
    fprintf (file, "\t.reg.u%d %%argp;\n", GET_MODE_BITSIZE (Pmode));

  fprintf (file, "\t.reg.u%d %s;\n", GET_MODE_BITSIZE (Pmode),
	   reg_names[OUTGOING_STATIC_CHAIN_REGNUM]);

  /* Declare the pseudos we have as ptx registers.  */
  int maxregs = max_reg_num ();
  for (int i = LAST_VIRTUAL_REGISTER + 1; i < maxregs; i++)
    {
      if (regno_reg_rtx[i] != const0_rtx)
	{
	  machine_mode mode = PSEUDO_REGNO_MODE (i);
	  int count = maybe_split_mode (&mode);
	  if (count > 1)
	    {
	      while (count-- > 0)
		fprintf (file, "\t.reg%s %%r%d$%d;\n",
			 nvptx_ptx_type_from_mode (mode, true),
			 i, count);
	    }
	  else
	    fprintf (file, "\t.reg%s %%r%d;\n",
		     nvptx_ptx_type_from_mode (mode, true),
		     i);
	}
    }

  /* The only reason we might be using outgoing args is if we call a stdargs
     function.  Allocate the space for this.  If we called varargs functions
     without passing any variadic arguments, we'll see a reference to outargs
     even with a zero outgoing_args_size.  */
  HOST_WIDE_INT sz = crtl->outgoing_args_size;
  if (sz == 0)
    sz = 1;
  if (cfun->machine->has_call_with_varargs)
    fprintf (file, "\t.reg.u%d %%outargs;\n"
	     "\t.local.align 8 .b8 %%outargs_ar["HOST_WIDE_INT_PRINT_DEC"];\n",
	     BITS_PER_WORD, sz);
  if (cfun->machine->punning_buffer_size > 0)
    fprintf (file, "\t.reg.u%d %%punbuffer;\n"
	     "\t.local.align 8 .b8 %%punbuffer_ar[%d];\n",
	     BITS_PER_WORD, cfun->machine->punning_buffer_size);

  /* Declare a local variable for the frame.  */
  sz = get_frame_size ();
  if (sz > 0 || cfun->machine->has_call_with_sc)
    {
      fprintf (file, "\t.reg.u%d %%frame;\n"
	       "\t.local.align 8 .b8 %%farray["HOST_WIDE_INT_PRINT_DEC"];\n",
	       BITS_PER_WORD, sz == 0 ? 1 : sz);
      fprintf (file, "\tcvta.local.u%d %%frame, %%farray;\n",
	       BITS_PER_WORD);
    }

  if (cfun->machine->has_call_with_varargs)
      fprintf (file, "\tcvta.local.u%d %%outargs, %%outargs_ar;\n",
	       BITS_PER_WORD);
  if (cfun->machine->punning_buffer_size > 0)
      fprintf (file, "\tcvta.local.u%d %%punbuffer, %%punbuffer_ar;\n",
	       BITS_PER_WORD);

  /* Now emit any copies necessary for arguments.  */
  walk_args_for_param (file, TYPE_ARG_TYPES (fntype), DECL_ARGUMENTS (decl),
		       true, return_in_mem);
  if (return_in_mem)
    fprintf (file, "ld.param.u%d %%ar1, [%%in_ar1];\n",
	     GET_MODE_BITSIZE (Pmode));
  if (stdarg_p (fntype))
    fprintf (file, "ld.param.u%d %%argp, [%%in_argp];\n",
	     GET_MODE_BITSIZE (Pmode));
}

/* Output a return instruction.  Also copy the return value to its outgoing
   location.  */

const char *
nvptx_output_return (void)
{
  tree fntype = TREE_TYPE (current_function_decl);
  tree result_type = TREE_TYPE (fntype);
  if (TYPE_MODE (result_type) != VOIDmode)
    {
      machine_mode mode = TYPE_MODE (result_type);
      if (RETURN_IN_REG_P (mode))
	{
	  mode = arg_promotion (mode);
	  fprintf (asm_out_file, "\tst.param%s\t[%%out_retval], %%retval;\n",
		   nvptx_ptx_type_from_mode (mode, false));
	}
    }

  return "ret;";
}

/* Construct a function declaration from a call insn.  This can be
   necessary for two reasons - either we have an indirect call which
   requires a .callprototype declaration, or we have a libcall
   generated by emit_library_call for which no decl exists.  */

static void
write_func_decl_from_insn (std::stringstream &s, rtx result, rtx pat,
			   rtx callee)
{
  bool callprototype = register_operand (callee, Pmode);
  const char *name = "_";
  if (!callprototype)
    {
      name = XSTR (callee, 0);
      name = nvptx_name_replacement (name);
      s << "// BEGIN GLOBAL FUNCTION DECL: " << name << "\n";
    }
  s << (callprototype ? "\t.callprototype\t" : "\t.extern .func ");

  if (result != NULL_RTX)
    {
      s << "(.param";
      s << nvptx_ptx_type_from_mode (arg_promotion (GET_MODE (result)),
				     false);
      s << " ";
      if (callprototype)
	s << "_";
      else
	s << "%out_retval";
      s << ")";
    }

  s << name;

  int nargs = XVECLEN (pat, 0) - 1;
  if (nargs > 0)
    {
      s << " (";
      for (int i = 0; i < nargs; i++)
	{
	  rtx t = XEXP (XVECEXP (pat, 0, i + 1), 0);
	  machine_mode mode = GET_MODE (t);
	  int count = maybe_split_mode (&mode);

	  while (count-- > 0)
	    {
	      s << ".param";
	      s << nvptx_ptx_type_from_mode (mode, false);
	      s << " ";
	      if (callprototype)
		s << "_";
	      else
		s << "%arg" << i;
	      if (mode == QImode || mode == HImode)
		s << "[1]";
	      if (i + 1 < nargs || count > 0)
		s << ", ";
	    }
	}
      s << ")";
    }
  s << ";\n";
}

/* Terminate a function by writing a closing brace to FILE.  */

void
nvptx_function_end (FILE *file)
{
  fprintf (file, "\t}\n");
}

/* Decide whether we can make a sibling call to a function.  For ptx, we
   can't.  */

static bool
nvptx_function_ok_for_sibcall (tree, tree)
{
  return false;
}

/* Implement the TARGET_CALL_ARGS hook.  Record information about one
   argument to the next call.  */

static void
nvptx_call_args (rtx arg, tree funtype)
{
  if (cfun->machine->start_call == NULL_RTX)
    {
      cfun->machine->call_args = NULL;
      cfun->machine->funtype = funtype;
      cfun->machine->start_call = const0_rtx;
    }
  if (arg == pc_rtx)
    return;

  rtx_expr_list *args_so_far = cfun->machine->call_args;
  if (REG_P (arg))
    cfun->machine->call_args = alloc_EXPR_LIST (VOIDmode, arg, args_so_far);
}

/* Implement the corresponding END_CALL_ARGS hook.  Clear and free the
   information we recorded.  */

static void
nvptx_end_call_args (void)
{
  cfun->machine->start_call = NULL_RTX;
  free_EXPR_LIST_list (&cfun->machine->call_args);
}

/* Emit the sequence for a call.  */

void
nvptx_expand_call (rtx retval, rtx address)
{
  int nargs;
  rtx callee = XEXP (address, 0);
  rtx pat, t;
  rtvec vec;
  bool external_decl = false;

  nargs = 0;
  for (t = cfun->machine->call_args; t; t = XEXP (t, 1))
    nargs++;

  bool has_varargs = false;
  tree decl_type = NULL_TREE;

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
	  decl_type = TREE_TYPE (decl);
	  if (DECL_STATIC_CHAIN (decl))
	    cfun->machine->has_call_with_sc = true;
	  if (DECL_EXTERNAL (decl))
	    external_decl = true;
	}
    }
  if (cfun->machine->funtype
      /* It's possible to construct testcases where we call a variable.
	 See compile/20020129-1.c.  stdarg_p will crash so avoid calling it
	 in such a case.  */
      && (TREE_CODE (cfun->machine->funtype) == FUNCTION_TYPE
	  || TREE_CODE (cfun->machine->funtype) == METHOD_TYPE)
      && stdarg_p (cfun->machine->funtype))
    {
      has_varargs = true;
      cfun->machine->has_call_with_varargs = true;
    }
  vec = rtvec_alloc (nargs + 1 + (has_varargs ? 1 : 0));
  pat = gen_rtx_PARALLEL (VOIDmode, vec);
  if (has_varargs)
    {
      rtx this_arg = gen_reg_rtx (Pmode);
      if (Pmode == DImode)
	emit_move_insn (this_arg, stack_pointer_rtx);
      else
	emit_move_insn (this_arg, stack_pointer_rtx);
      XVECEXP (pat, 0, nargs + 1) = gen_rtx_USE (VOIDmode, this_arg);
    }

  int i;
  rtx arg;
  for (i = 1, arg = cfun->machine->call_args; arg; arg = XEXP (arg, 1), i++)
    {
      rtx this_arg = XEXP (arg, 0);
      XVECEXP (pat, 0, i) = gen_rtx_USE (VOIDmode, this_arg);
    }

  rtx tmp_retval = retval;
  t = gen_rtx_CALL (VOIDmode, address, const0_rtx);
  if (retval != NULL_RTX)
    {
      if (!nvptx_register_operand (retval, GET_MODE (retval)))
	tmp_retval = gen_reg_rtx (GET_MODE (retval));
      t = gen_rtx_SET (VOIDmode, tmp_retval, t);
    }
  XVECEXP (pat, 0, 0) = t;
  if (!REG_P (callee)
      && (decl_type == NULL_TREE
	  || (external_decl && TYPE_ARG_TYPES (decl_type) == NULL_TREE)))
    {
      rtx *slot = declared_libfuncs_htab->find_slot (callee, INSERT);
      if (*slot == NULL)
	{
	  *slot = callee;
	  write_func_decl_from_insn (func_decls, retval, pat, callee);
	}
    }
  emit_call_insn (pat);
  if (tmp_retval != retval)
    emit_move_insn (retval, tmp_retval);
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
nvptx_function_arg (cumulative_args_t, machine_mode mode,
		    const_tree, bool named)
{
  if (mode == VOIDmode)
    return NULL_RTX;

  if (named)
    return gen_reg_rtx (mode);
  return NULL_RTX;
}

/* Implement TARGET_FUNCTION_INCOMING_ARG.  */

static rtx
nvptx_function_incoming_arg (cumulative_args_t cum_v, machine_mode mode,
			     const_tree, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  if (mode == VOIDmode)
    return NULL_RTX;

  if (!named)
    return NULL_RTX;

  /* No need to deal with split modes here, the only case that can
     happen is complex modes and those are dealt with by
     TARGET_SPLIT_COMPLEX_ARG.  */
  return gen_rtx_UNSPEC (mode,
			 gen_rtvec (1, GEN_INT (1 + cum->count)),
			 UNSPEC_ARG_REG);
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */

static void
nvptx_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			    const_tree type ATTRIBUTE_UNUSED,
			    bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  if (mode == TImode)
    cum->count += 2;
  else
    cum->count++;
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

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  */

static unsigned int
nvptx_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int boundary = type ? TYPE_ALIGN (type) : GET_MODE_BITSIZE (mode);

  if (boundary > BITS_PER_WORD)
    return 2 * BITS_PER_WORD;

  if (mode == BLKmode)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if (size > 4)
        return 2 * BITS_PER_WORD;
      if (boundary < BITS_PER_WORD)
        {
          if (size >= 3)
            return BITS_PER_WORD;
          if (size >= 2)
            return 2 * BITS_PER_UNIT;
        }
    }
  return boundary;
}

/* TARGET_FUNCTION_VALUE implementation.  Returns an RTX representing the place
   where function FUNC returns or receives a value of data type TYPE.  */

static rtx
nvptx_function_value (const_tree type, const_tree func ATTRIBUTE_UNUSED,
		      bool outgoing)
{
  int unsignedp = TYPE_UNSIGNED (type);
  machine_mode orig_mode = TYPE_MODE (type);
  machine_mode mode = promote_function_mode (type, orig_mode,
					     &unsignedp, NULL_TREE, 1);
  if (outgoing)
    return gen_rtx_REG (mode, NVPTX_RETURN_REGNUM);
  if (cfun->machine->start_call == NULL_RTX)
    /* Pretend to return in a hard reg for early uses before pseudos can be
       generated.  */
    return gen_rtx_REG (mode, NVPTX_RETURN_REGNUM);
  return gen_reg_rtx (mode);
}

/* Implement TARGET_LIBCALL_VALUE.  */

static rtx
nvptx_libcall_value (machine_mode mode, const_rtx)
{
  if (cfun->machine->start_call == NULL_RTX)
    /* Pretend to return in a hard reg for early uses before pseudos can be
       generated.  */
    return gen_rtx_REG (mode, NVPTX_RETURN_REGNUM);
  return gen_reg_rtx (mode);
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
nvptx_pass_by_reference (cumulative_args_t, machine_mode mode,
			 const_tree type, bool)
{
  return !PASS_IN_REG_P (mode, type);
}

/* Implement TARGET_RETURN_IN_MEMORY.  */

static bool
nvptx_return_in_memory (const_tree type, const_tree)
{
  machine_mode mode = TYPE_MODE (type);
  if (!RETURN_IN_REG_P (mode))
    return true;
  return false;
}

/* Implement TARGET_PROMOTE_FUNCTION_MODE.  */

static machine_mode
nvptx_promote_function_mode (const_tree type, machine_mode mode,
			     int *punsignedp,
			     const_tree funtype, int for_return)
{
  if (type == NULL_TREE)
    return mode;
  if (for_return)
    return promote_mode (type, mode, punsignedp);
  /* For K&R-style functions, try to match the language promotion rules to
     minimize type mismatches at assembly time.  */
  if (TYPE_ARG_TYPES (funtype) == NULL_TREE
      && type != NULL_TREE
      && !AGGREGATE_TYPE_P (type))
    {
      if (mode == SFmode)
	mode = DFmode;
      mode = arg_promotion (mode);
    }

  return mode;
}

/* Implement TARGET_STATIC_CHAIN.  */

static rtx
nvptx_static_chain (const_tree fndecl, bool incoming_p)
{
  if (!DECL_STATIC_CHAIN (fndecl))
    return NULL;

  if (incoming_p)
    return gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);
  else
    return gen_rtx_REG (Pmode, OUTGOING_STATIC_CHAIN_REGNUM);
}

/* Emit a comparison COMPARE, and return the new test to be used in the
   jump.  */

rtx
nvptx_expand_compare (rtx compare)
{
  rtx pred = gen_reg_rtx (BImode);
  rtx cmp = gen_rtx_fmt_ee (GET_CODE (compare), BImode,
			    XEXP (compare, 0), XEXP (compare, 1));
  emit_insn (gen_rtx_SET (VOIDmode, pred, cmp));
  return gen_rtx_NE (BImode, pred, const0_rtx);
}

/* When loading an operand ORIG_OP, verify whether an address space
   conversion to generic is required, and if so, perform it.  Also
   check for SYMBOL_REFs for function decls and call
   nvptx_record_needed_fndecl as needed.
   Return either the original operand, or the converted one.  */

rtx
nvptx_maybe_convert_symbolic_operand (rtx orig_op)
{
  if (GET_MODE (orig_op) != Pmode)
    return orig_op;

  rtx op = orig_op;
  while (GET_CODE (op) == PLUS || GET_CODE (op) == CONST)
    op = XEXP (op, 0);
  if (GET_CODE (op) != SYMBOL_REF)
    return orig_op;

  tree decl = SYMBOL_REF_DECL (op);
  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    {
      nvptx_record_needed_fndecl (decl);
      return orig_op;
    }

  addr_space_t as = nvptx_addr_space_from_address (op);
  if (as == ADDR_SPACE_GENERIC)
    return orig_op;

  enum unspec code;
  code = (as == ADDR_SPACE_GLOBAL ? UNSPEC_FROM_GLOBAL
	  : as == ADDR_SPACE_LOCAL ? UNSPEC_FROM_LOCAL
	  : as == ADDR_SPACE_SHARED ? UNSPEC_FROM_SHARED
	  : as == ADDR_SPACE_CONST ? UNSPEC_FROM_CONST
	  : UNSPEC_FROM_PARAM);
  rtx dest = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (VOIDmode, dest,
			  gen_rtx_UNSPEC (Pmode, gen_rtvec (1, orig_op),
					  code)));
  return dest;
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

/* Implement HARD_REGNO_MODE_OK.  We barely use hard regs, but we want
   to ensure that the return register's mode isn't changed.  */

bool
nvptx_hard_regno_mode_ok (int regno, machine_mode mode)
{
  if (regno != NVPTX_RETURN_REGNUM
      || cfun == NULL || cfun->machine->ret_reg_mode == VOIDmode)
    return true;
  return mode == cfun->machine->ret_reg_mode;
}

/* Convert an address space AS to the corresponding ptx string.  */

const char *
nvptx_section_from_addr_space (addr_space_t as)
{
  switch (as)
    {
    case ADDR_SPACE_CONST:
      return ".const";

    case ADDR_SPACE_GLOBAL:
      return ".global";

    case ADDR_SPACE_SHARED:
      return ".shared";

    case ADDR_SPACE_GENERIC:
      return "";

    default:
      gcc_unreachable ();
    }
}

/* Determine whether DECL goes into .const or .global.  */

const char *
nvptx_section_for_decl (const_tree decl)
{
  bool is_const = (CONSTANT_CLASS_P (decl)
		   || TREE_CODE (decl) == CONST_DECL
		   || TREE_READONLY (decl));
  if (is_const)
    return ".const";

  return ".global";
}

/* Look for a SYMBOL_REF in ADDR and return the address space to be used
   for the insn referencing this address.  */

addr_space_t
nvptx_addr_space_from_address (rtx addr)
{
  while (GET_CODE (addr) == PLUS || GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) != SYMBOL_REF)
    return ADDR_SPACE_GENERIC;

  tree decl = SYMBOL_REF_DECL (addr);
  if (decl == NULL_TREE || TREE_CODE (decl) == FUNCTION_DECL)
    return ADDR_SPACE_GENERIC;

  bool is_const = (CONSTANT_CLASS_P (decl)
		   || TREE_CODE (decl) == CONST_DECL
		   || TREE_READONLY (decl));
  if (is_const)
    return ADDR_SPACE_CONST;

  return ADDR_SPACE_GLOBAL;
}

/* Machinery to output constant initializers.  */

/* Used when assembling integers to ensure data is emitted in
   pieces whose size matches the declaration we printed.  */
static unsigned int decl_chunk_size;
static machine_mode decl_chunk_mode;
/* Used in the same situation, to keep track of the byte offset
   into the initializer.  */
static unsigned HOST_WIDE_INT decl_offset;
/* The initializer part we are currently processing.  */
static HOST_WIDE_INT init_part;
/* The total size of the object.  */
static unsigned HOST_WIDE_INT object_size;
/* True if we found a skip extending to the end of the object.  Used to
   assert that no data follows.  */
static bool object_finished;

/* Write the necessary separator string to begin a new initializer value.  */

static void
begin_decl_field (void)
{
  /* We never see decl_offset at zero by the time we get here.  */
  if (decl_offset == decl_chunk_size)
    fprintf (asm_out_file, " = { ");
  else
    fprintf (asm_out_file, ", ");
}

/* Output the currently stored chunk as an initializer value.  */

static void
output_decl_chunk (void)
{
  begin_decl_field ();
  output_address (gen_int_mode (init_part, decl_chunk_mode));
  init_part = 0;
}

/* Add value VAL sized SIZE to the data we're emitting, and keep writing
   out chunks as they fill up.  */

static void
nvptx_assemble_value (HOST_WIDE_INT val, unsigned int size)
{
  unsigned HOST_WIDE_INT chunk_offset = decl_offset % decl_chunk_size;
  gcc_assert (!object_finished);
  while (size > 0)
    {
      int this_part = size;
      if (chunk_offset + this_part > decl_chunk_size)
	this_part = decl_chunk_size - chunk_offset;
      HOST_WIDE_INT val_part;
      HOST_WIDE_INT mask = 2;
      mask <<= this_part * BITS_PER_UNIT - 1;
      val_part = val & (mask - 1);
      init_part |= val_part << (BITS_PER_UNIT * chunk_offset);
      val >>= BITS_PER_UNIT * this_part;
      size -= this_part;
      decl_offset += this_part;
      if (decl_offset % decl_chunk_size == 0)
	output_decl_chunk ();

      chunk_offset = 0;
    }
}

/* Target hook for assembling integer object X of size SIZE.  */

static bool
nvptx_assemble_integer (rtx x, unsigned int size, int ARG_UNUSED (aligned_p))
{
  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST)
    {
      gcc_assert (size = decl_chunk_size);
      if (decl_offset % decl_chunk_size != 0)
	sorry ("cannot emit unaligned pointers in ptx assembly");
      decl_offset += size;
      begin_decl_field ();

      HOST_WIDE_INT off = 0;
      if (GET_CODE (x) == CONST)
	x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS)
	{
	  off = INTVAL (XEXP (x, 1));
	  x = XEXP (x, 0);
	}
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  nvptx_record_needed_fndecl (SYMBOL_REF_DECL (x));
	  fprintf (asm_out_file, "generic(");
	  output_address (x);
	  fprintf (asm_out_file, ")");
	}
      if (off != 0)
	fprintf (asm_out_file, " + " HOST_WIDE_INT_PRINT_DEC, off);
      return true;
    }

  HOST_WIDE_INT val;
  switch (GET_CODE (x))
    {
    case CONST_INT:
      val = INTVAL (x);
      break;
    case CONST_DOUBLE:
      gcc_unreachable ();
      break;
    default:
      gcc_unreachable ();
    }

  nvptx_assemble_value (val, size);
  return true;
}

/* Output SIZE zero bytes.  We ignore the FILE argument since the
   functions we're calling to perform the output just use
   asm_out_file.  */

void
nvptx_output_skip (FILE *, unsigned HOST_WIDE_INT size)
{
  if (decl_offset + size >= object_size)
    {
      if (decl_offset % decl_chunk_size != 0)
	nvptx_assemble_value (0, decl_chunk_size);
      object_finished = true;
      return;
    }

  while (size > decl_chunk_size)
    {
      nvptx_assemble_value (0, decl_chunk_size);
      size -= decl_chunk_size;
    }
  while (size-- > 0)
    nvptx_assemble_value (0, 1);
}

/* Output a string STR with length SIZE.  As in nvptx_output_skip we
   ignore the FILE arg.  */

void
nvptx_output_ascii (FILE *, const char *str, unsigned HOST_WIDE_INT size)
{
  for (unsigned HOST_WIDE_INT i = 0; i < size; i++)
    nvptx_assemble_value (str[i], 1);
}

/* Called when the initializer for a decl has been completely output through
   combinations of the three functions above.  */

static void
nvptx_assemble_decl_end (void)
{
  if (decl_offset != 0)
    {
      if (!object_finished && decl_offset % decl_chunk_size != 0)
	nvptx_assemble_value (0, decl_chunk_size);

      fprintf (asm_out_file, " }");
    }
  fprintf (asm_out_file, ";\n");
}

/* Start a declaration of a variable of TYPE with NAME to
   FILE.  IS_PUBLIC says whether this will be externally visible.
   Here we just write the linker hint and decide on the chunk size
   to use.  */

static void
init_output_initializer (FILE *file, const char *name, const_tree type,
			 bool is_public)
{
  fprintf (file, "// BEGIN%s VAR DEF: ", is_public ? " GLOBAL" : "");
  assemble_name_raw (file, name);
  fputc ('\n', file);

  if (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);
  int sz = int_size_in_bytes (type);
  if ((TREE_CODE (type) != INTEGER_TYPE
       && TREE_CODE (type) != ENUMERAL_TYPE
       && TREE_CODE (type) != REAL_TYPE)
      || sz < 0
      || sz > HOST_BITS_PER_WIDE_INT)
    type = ptr_type_node;
  decl_chunk_size = int_size_in_bytes (type);
  decl_chunk_mode = int_mode_for_mode (TYPE_MODE (type));
  decl_offset = 0;
  init_part = 0;
  object_finished = false;
}

/* Implement TARGET_ASM_DECLARE_CONSTANT_NAME.  Begin the process of
   writing a constant variable EXP with NAME and SIZE and its
   initializer to FILE.  */

static void
nvptx_asm_declare_constant_name (FILE *file, const char *name,
				 const_tree exp, HOST_WIDE_INT size)
{
  tree type = TREE_TYPE (exp);
  init_output_initializer (file, name, type, false);
  fprintf (file, "\t.const .align %d .u%d ",
	   TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT,
	   decl_chunk_size * BITS_PER_UNIT);
  assemble_name (file, name);
  fprintf (file, "[" HOST_WIDE_INT_PRINT_DEC "]",
	   (size + decl_chunk_size - 1) / decl_chunk_size);
  object_size = size;
}

/* Implement the ASM_DECLARE_OBJECT_NAME macro.  Used to start writing
   a variable DECL with NAME to FILE.  */

void
nvptx_declare_object_name (FILE *file, const char *name, const_tree decl)
{
  if (decl && DECL_SIZE (decl))
    {
      tree type = TREE_TYPE (decl);
      unsigned HOST_WIDE_INT size;

      init_output_initializer (file, name, type, TREE_PUBLIC (decl));
      size = tree_to_uhwi (DECL_SIZE_UNIT (decl));
      const char *section = nvptx_section_for_decl (decl);
      fprintf (file, "\t%s%s .align %d .u%d ",
	       TREE_PUBLIC (decl) ? " .visible" : "", section,
	       DECL_ALIGN (decl) / BITS_PER_UNIT,
	       decl_chunk_size * BITS_PER_UNIT);
      assemble_name (file, name);
      if (size > 0)
	fprintf (file, "[" HOST_WIDE_INT_PRINT_DEC "]",
		 (size + decl_chunk_size - 1) / decl_chunk_size);
      else
	object_finished = true;
      object_size = size;
    }
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
  if (TREE_CODE (decl) != VAR_DECL)
    return;
  const char *section = nvptx_section_for_decl (decl);
  fprintf (file, "// BEGIN%s VAR DECL: ", TREE_PUBLIC (decl) ? " GLOBAL" : "");
  assemble_name_raw (file, name);
  fputs ("\n", file);
  HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (decl));
  fprintf (file, ".extern %s .b8 ", section);
  assemble_name_raw (file, name);
  if (size > 0)
    fprintf (file, "["HOST_WIDE_INT_PRINT_DEC"]", size);
  fprintf (file, ";\n\n");
}

/* Output INSN, which is a call to CALLEE with result RESULT.  For ptx, this
   involves writing .param declarations and in/out copies into them.  */

const char *
nvptx_output_call_insn (rtx_insn *insn, rtx result, rtx callee)
{
  char buf[256];
  static int labelno;
  bool needs_tgt = register_operand (callee, Pmode);
  rtx pat = PATTERN (insn);
  int nargs = XVECLEN (pat, 0) - 1;
  tree decl = NULL_TREE;

  fprintf (asm_out_file, "\t{\n");
  if (result != NULL)
    {
      fprintf (asm_out_file, "\t\t.param%s %%retval_in;\n",
	       nvptx_ptx_type_from_mode (arg_promotion (GET_MODE (result)),
					 false));
    }

  if (GET_CODE (callee) == SYMBOL_REF)
    {
      decl = SYMBOL_REF_DECL (callee);
      if (decl && DECL_EXTERNAL (decl))
	nvptx_record_fndecl (decl);
    }

  if (needs_tgt)
    {
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCT", labelno);
      labelno++;
      ASM_OUTPUT_LABEL (asm_out_file, buf);
      std::stringstream s;
      write_func_decl_from_insn (s, result, pat, callee);
      fputs (s.str().c_str(), asm_out_file);
    }

  for (int i = 0, argno = 0; i < nargs; i++)
    {
      rtx t = XEXP (XVECEXP (pat, 0, i + 1), 0);
      machine_mode mode = GET_MODE (t);
      int count = maybe_split_mode (&mode);

      while (count-- > 0)
	fprintf (asm_out_file, "\t\t.param%s %%out_arg%d%s;\n",
		 nvptx_ptx_type_from_mode (mode, false), argno++,
		 mode == QImode || mode == HImode ? "[1]" : "");
    }
  for (int i = 0, argno = 0; i < nargs; i++)
    {
      rtx t = XEXP (XVECEXP (pat, 0, i + 1), 0);
      gcc_assert (REG_P (t));
      machine_mode mode = GET_MODE (t);
      int count = maybe_split_mode (&mode);

      if (count == 1)
	fprintf (asm_out_file, "\t\tst.param%s [%%out_arg%d], %%r%d;\n",
		 nvptx_ptx_type_from_mode (mode, false), argno++,
		 REGNO (t));
      else
	{
	  int n = 0;
	  while (count-- > 0)
	    fprintf (asm_out_file, "\t\tst.param%s [%%out_arg%d], %%r%d$%d;\n",
		     nvptx_ptx_type_from_mode (mode, false), argno++,
		     REGNO (t), n++);
	}
    }

  fprintf (asm_out_file, "\t\tcall ");
  if (result != NULL_RTX)
    fprintf (asm_out_file, "(%%retval_in), ");

  if (decl)
    {
      const char *name = get_fnname_from_decl (decl);
      name = nvptx_name_replacement (name);
      assemble_name (asm_out_file, name);
    }
  else
    output_address (callee);

  if (nargs > 0 || (decl && DECL_STATIC_CHAIN (decl)))
    {
      fprintf (asm_out_file, ", (");
      int i, argno;
      for (i = 0, argno = 0; i < nargs; i++)
	{
	  rtx t = XEXP (XVECEXP (pat, 0, i + 1), 0);
	  machine_mode mode = GET_MODE (t);
	  int count = maybe_split_mode (&mode);

	  while (count-- > 0)
	    {
	      fprintf (asm_out_file, "%%out_arg%d", argno++);
	      if (i + 1 < nargs || count > 0)
		fprintf (asm_out_file, ", ");
	    }
	}
      if (decl && DECL_STATIC_CHAIN (decl))
	{
	  if (i > 0)
	    fprintf (asm_out_file, ", ");
	  fprintf (asm_out_file, "%s",
		   reg_names [OUTGOING_STATIC_CHAIN_REGNUM]);
	}

      fprintf (asm_out_file, ")");
    }
  if (needs_tgt)
    {
      fprintf (asm_out_file, ", ");
      assemble_name (asm_out_file, buf);
    }
  fprintf (asm_out_file, ";\n");
  if (result != NULL_RTX)
    return "ld.param%t0\t%0, [%%retval_in];\n\t}";

  return "}";
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
nvptx_print_operand_punct_valid_p (unsigned char c)
{
  return c == '.' || c== '#';
}

static void nvptx_print_operand (FILE *, rtx, int);

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
      output_address (XEXP (x, 0));
      fprintf (file, "+");
      output_address (off);
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
nvptx_print_operand_address (FILE *file, rtx addr)
{
  nvptx_print_address_operand (file, addr, VOIDmode);
}

/* Print an operand, X, to FILE, with an optional modifier in CODE.

   Meaning of CODE:
   . -- print the predicate for the instruction or an emptry string for an
        unconditional one.
   # -- print a rounding mode for the instruction

   A -- print an address space identifier for a MEM
   c -- print an opcode suffix for a comparison operator, including a type code
   d -- print a CONST_INT as a vector dimension (x, y, or z)
   f -- print a full reg even for something that must always be split
   t -- print a type opcode suffix, promoting QImode to 32 bits
   T -- print a type size in bits
   u -- print a type opcode suffix without promotions.  */

static void
nvptx_print_operand (FILE *file, rtx x, int code)
{
  rtx orig_x = x;
  machine_mode op_mode;

  if (code == '.')
    {
      x = current_insn_predicate;
      if (x)
	{
	  unsigned int regno = REGNO (XEXP (x, 0));
	  fputs ("[", file);
	  if (GET_CODE (x) == EQ)
	    fputs ("!", file);
	  fputs (reg_names [regno], file);
	  fputs ("]", file);
	}
      return;
    }
  else if (code == '#')
    {
      fputs (".rn", file);
      return;
    }

  enum rtx_code x_code = GET_CODE (x);

  switch (code)
    {
    case 'A':
      {
	addr_space_t as = nvptx_addr_space_from_address (XEXP (x, 0));
	fputs (nvptx_section_from_addr_space (as), file);
      }
      break;

    case 'd':
      gcc_assert (x_code == CONST_INT);
      if (INTVAL (x) == 0)
	fputs (".x", file);
      else if (INTVAL (x) == 1)
	fputs (".y", file);
      else if (INTVAL (x) == 2)
	fputs (".z", file);
      else
	gcc_unreachable ();
      break;

    case 't':
      op_mode = nvptx_underlying_object_mode (x);
      fprintf (file, "%s", nvptx_ptx_type_from_mode (op_mode, true));
      break;

    case 'u':
      op_mode = nvptx_underlying_object_mode (x);
      fprintf (file, "%s", nvptx_ptx_type_from_mode (op_mode, false));
      break;

    case 'T':
      fprintf (file, "%d", GET_MODE_BITSIZE (GET_MODE (x)));
      break;

    case 'j':
      fprintf (file, "@");
      goto common;

    case 'J':
      fprintf (file, "@!");
      goto common;

    case 'c':
      op_mode = GET_MODE (XEXP (x, 0));
      switch (x_code)
	{
	case EQ:
	  fputs (".eq", file);
	  break;
	case NE:
	  if (FLOAT_MODE_P (op_mode))
	    fputs (".neu", file);
	  else
	    fputs (".ne", file);
	  break;
	case LE:
	  fputs (".le", file);
	  break;
	case GE:
	  fputs (".ge", file);
	  break;
	case LT:
	  fputs (".lt", file);
	  break;
	case GT:
	  fputs (".gt", file);
	  break;
	case LEU:
	  fputs (".ls", file);
	  break;
	case GEU:
	  fputs (".hs", file);
	  break;
	case LTU:
	  fputs (".lo", file);
	  break;
	case GTU:
	  fputs (".hi", file);
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
      if (FLOAT_MODE_P (op_mode)
	  || x_code == EQ || x_code == NE
	  || x_code == GEU || x_code == GTU
	  || x_code == LEU || x_code == LTU)
	fputs (nvptx_ptx_type_from_mode (op_mode, true), file);
      else
	fprintf (file, ".s%d", GET_MODE_BITSIZE (op_mode));
      break;
    default:
    common:
      switch (x_code)
	{
	case SUBREG:
	  x = SUBREG_REG (x);
	  /* fall through */

	case REG:
	  if (HARD_REGISTER_P (x))
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	  else
	    fprintf (file, "%%r%d", REGNO (x));
	  if (code != 'f' && nvptx_split_reg_p (GET_MODE (x)))
	    {
	      gcc_assert (GET_CODE (orig_x) == SUBREG
			  && !nvptx_split_reg_p (GET_MODE (orig_x)));
	      fprintf (file, "$%d", SUBREG_BYTE (orig_x) / UNITS_PER_WORD);
	    }
	  break;

	case MEM:
	  fputc ('[', file);
	  nvptx_print_address_operand (file, XEXP (x, 0), GET_MODE (x));
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
	  REAL_VALUE_TYPE real;
	  REAL_VALUE_FROM_CONST_DOUBLE (real, x);
	  real_to_target (vals, &real, GET_MODE (x));
	  vals[0] &= 0xffffffff;
	  vals[1] &= 0xffffffff;
	  if (GET_MODE (x) == SFmode)
	    fprintf (file, "0f%08lx", vals[0]);
	  else
	    fprintf (file, "0d%08lx%08lx", vals[1], vals[0]);
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
nvptx_reorg (void)
{
  struct reg_replace qiregs, hiregs, siregs, diregs;
  rtx_insn *insn, *next;

  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  df_clear_flags (DF_LR_RUN_DCE);
  df_analyze ();

  thread_prologue_and_epilogue_insns ();

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
	  || asm_noperands (insn) >= 0
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

	      rtx pat = gen_rtx_SET (VOIDmode, new_reg,
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

	      rtx pat = gen_rtx_SET (VOIDmode, inner,
				     gen_rtx_fmt_e (code, inner_mode, new_reg));
	      emit_insn_after (pat, insn);
	    }
	  validate_change (insn, recog_data.operand_loc[i], new_reg, false);
	}
    }

  int maxregs = max_reg_num ();
  regstat_init_n_sets_and_refs ();

  for (int i = LAST_VIRTUAL_REGISTER + 1; i < maxregs; i++)
    if (REG_N_SETS (i) == 0 && REG_N_REFS (i) == 0)
      regno_reg_rtx[i] = const0_rtx;
  regstat_free_n_sets_and_refs ();
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

  else if (TREE_TYPE (TREE_TYPE (decl)) != void_type_node)
    {
      error ("%qE attribute requires a void return type", name);
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
  { NULL, 0, 0, false, false, false, NULL, false }
};

/* Limit vector alignments to BIGGEST_ALIGNMENT.  */

static HOST_WIDE_INT
nvptx_vector_alignment (const_tree type)
{
  HOST_WIDE_INT align = tree_to_shwi (TYPE_SIZE (type));

  return MIN (align, BIGGEST_ALIGNMENT);
}

/* Record a symbol for mkoffload to enter into the mapping table.  */

static void
nvptx_record_offload_symbol (tree decl)
{
  fprintf (asm_out_file, "//:%s_MAP %s\n",
	   TREE_CODE (decl) == VAR_DECL ? "VAR" : "FUNC",
	   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
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

/* Write out the function declarations we've collected.  */

static void
nvptx_file_end (void)
{
  hash_table<tree_hasher>::iterator iter;
  tree decl;
  FOR_EACH_HASH_TABLE_ELEMENT (*needed_fndecls_htab, decl, tree, iter)
    nvptx_record_fndecl (decl, true);
  fputs (func_decls.str().c_str(), asm_out_file);
}

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nvptx_option_override

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE nvptx_attribute_table

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
#undef TARGET_FUNCTION_ARG_ROUND_BOUNDARY
#define TARGET_FUNCTION_ARG_ROUND_BOUNDARY nvptx_function_arg_boundary
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
#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG hook_bool_const_tree_true
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nvptx_return_in_memory
#undef TARGET_OMIT_STRUCT_RETURN_REG
#define TARGET_OMIT_STRUCT_RETURN_REG true
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING nvptx_strict_argument_naming
#undef TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN nvptx_static_chain

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

#undef TARGET_RECORD_OFFLOAD_SYMBOL
#define TARGET_RECORD_OFFLOAD_SYMBOL nvptx_record_offload_symbol

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT nvptx_vector_alignment

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-nvptx.h"
