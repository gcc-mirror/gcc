/* Output routines for Visium.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by C.Nettleton, J.P.Parkes and P.Garbett.

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
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple-expr.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "explow.h"
#include "expr.h"
#include "gimplify.h"
#include "langhooks.h"
#include "reload.h"
#include "tm-constrs.h"
#include "params.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* Enumeration of indexes into machine_libfunc_table.  */
enum machine_libfunc_index
{
  MLTI_long_int_memcpy,
  MLTI_wrd_memcpy,
  MLTI_byt_memcpy,

  MLTI_long_int_memset,
  MLTI_wrd_memset,
  MLTI_byt_memset,

  MLTI_set_trampoline_parity,

  MLTI_MAX
};

struct GTY(()) machine_libfuncs
{
  rtx table[MLTI_MAX];
};

/* The table of Visium-specific libfuncs.  */
static GTY(()) struct machine_libfuncs visium_libfuncs;

#define vlt visium_libfuncs.table

/* Accessor macros for visium_libfuncs.  */
#define long_int_memcpy_libfunc		(vlt[MLTI_long_int_memcpy])
#define wrd_memcpy_libfunc		(vlt[MLTI_wrd_memcpy])
#define byt_memcpy_libfunc		(vlt[MLTI_byt_memcpy])
#define long_int_memset_libfunc		(vlt[MLTI_long_int_memset])
#define wrd_memset_libfunc		(vlt[MLTI_wrd_memset])
#define byt_memset_libfunc		(vlt[MLTI_byt_memset])
#define set_trampoline_parity_libfunc	(vlt[MLTI_set_trampoline_parity])

/* Machine specific function data. */
struct GTY (()) machine_function
{
  /* Size of the frame of the function.  */
  int frame_size;

  /* Size of the reg parm save area, non-zero only for functions with variable
     argument list.  We cannot use the crtl->args.pretend_args_size machinery
     for this purpose because this size is added to virtual_incoming_args_rtx
     to give the location of the first parameter passed by the caller on the
     stack and virtual_incoming_args_rtx is also the location of the first
     parameter on the stack.  So crtl->args.pretend_args_size can be non-zero
     only if the first non-register named parameter is not passed entirely on
     the stack and this runs afoul of the need to have a reg parm save area
     even with a variable argument list starting on the stack because of the
     separate handling of general and floating-point registers.  */
  int reg_parm_save_area_size;

  /* True if we have created an rtx which relies on the frame pointer.  */
  bool frame_needed;

  /* True if we have exposed the flags register.  From this moment on, we
     cannot generate simple operations for integer registers.  We could
     use reload_completed for this purpose, but this would cripple the
     postreload CSE and GCSE passes which run before postreload split.  */
  bool flags_exposed;
};

#define visium_frame_size		cfun->machine->frame_size
#define visium_reg_parm_save_area_size 	cfun->machine->reg_parm_save_area_size
#define visium_frame_needed		cfun->machine->frame_needed
#define visium_flags_exposed		cfun->machine->flags_exposed

/* 1 if the next opcode is to be specially indented.  */
int visium_indent_opcode = 0;

/* Register number used for long branches when LR isn't available.  It
   must be a call-used register since it isn't saved on function entry.
   We do not care whether the branch is predicted or not on the GR6,
   given how unlikely it is to have a long branch in a leaf function.  */
static unsigned int long_branch_regnum = 31;

static tree visium_handle_interrupt_attr (tree *, tree, tree, int, bool *);
static inline bool current_function_saves_fp (void);
static inline bool current_function_saves_lr (void);
static inline bool current_function_has_lr_slot (void);

/* Supported attributes:
   interrupt -- specifies this function is an interrupt handler.   */
static const struct attribute_spec visium_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  {"interrupt", 0, 0, true, false, false, visium_handle_interrupt_attr, false},
  {NULL, 0, 0, false, false, false, NULL, false}
};

static struct machine_function *visium_init_machine_status (void);

/* Target hooks and TARGET_INITIALIZER  */

static bool visium_pass_by_reference (cumulative_args_t, machine_mode,
				      const_tree, bool);

static rtx visium_function_arg (cumulative_args_t, machine_mode,
				const_tree, bool);

static void visium_function_arg_advance (cumulative_args_t, machine_mode,
					 const_tree, bool);

static bool visium_return_in_memory (const_tree, const_tree fntype);

static rtx visium_function_value (const_tree, const_tree fn_decl_or_type,
				  bool);

static rtx visium_libcall_value (machine_mode, const_rtx);

static void visium_setup_incoming_varargs (cumulative_args_t,
					   machine_mode,
					   tree, int *, int);

static void visium_va_start (tree valist, rtx nextarg);

static tree visium_gimplify_va_arg (tree, tree, gimple_seq *, gimple_seq *);

static bool visium_function_ok_for_sibcall (tree, tree);

static bool visium_frame_pointer_required (void);

static tree visium_build_builtin_va_list (void);

static rtx_insn *visium_md_asm_adjust (vec<rtx> &, vec<rtx> &,
				       vec<const char *> &,
				       vec<rtx> &, HARD_REG_SET &);

static bool visium_legitimate_constant_p (machine_mode, rtx);

static bool visium_legitimate_address_p (machine_mode, rtx, bool);

static bool visium_print_operand_punct_valid_p (unsigned char);
static void visium_print_operand (FILE *, rtx, int);
static void visium_print_operand_address (FILE *, machine_mode, rtx);

static void visium_conditional_register_usage (void);

static rtx visium_legitimize_address (rtx, rtx, machine_mode);

static reg_class_t visium_secondary_reload (bool, rtx, reg_class_t,
					    machine_mode,
					    secondary_reload_info *);

static bool visium_class_likely_spilled_p (reg_class_t);

static void visium_trampoline_init (rtx, tree, rtx);

static int visium_issue_rate (void);

static int visium_adjust_priority (rtx_insn *, int);

static int visium_adjust_cost (rtx_insn *, int, rtx_insn *, int, unsigned int);

static int visium_register_move_cost (machine_mode, reg_class_t,
				      reg_class_t);

static int visium_memory_move_cost (machine_mode, reg_class_t, bool);

static bool visium_rtx_costs (rtx, machine_mode, int, int, int *, bool);

static void visium_option_override (void);

static void visium_init_libfuncs (void);

static unsigned int visium_reorg (void);

static bool visium_hard_regno_mode_ok (unsigned int, machine_mode);

static bool visium_modes_tieable_p (machine_mode, machine_mode);

/* Setup the global target hooks structure.  */

#undef  TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 31

#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE visium_pass_by_reference

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG visium_function_arg

#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE visium_function_arg_advance

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY visium_return_in_memory

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE visium_function_value

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE visium_libcall_value

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS visium_setup_incoming_varargs

#undef  TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START visium_va_start

#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST visium_build_builtin_va_list

#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR visium_gimplify_va_arg

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P visium_legitimate_constant_p

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P visium_legitimate_address_p

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P visium_print_operand_punct_valid_p
#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND visium_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS visium_print_operand_address

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE visium_attribute_table

#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0

#undef  TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE visium_issue_rate

#undef  TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY visium_adjust_priority

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST visium_adjust_cost

#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST visium_memory_move_cost

#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST visium_register_move_cost

#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS visium_rtx_costs

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL visium_function_ok_for_sibcall

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED visium_frame_pointer_required

#undef  TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD visium_secondary_reload

#undef  TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P visium_class_likely_spilled_p

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS visium_legitimize_address

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE visium_option_override

#undef  TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS visium_init_libfuncs

#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE visium_conditional_register_usage

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT visium_trampoline_init

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST visium_md_asm_adjust

#undef TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM FLAGS_REGNUM

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK visium_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P visium_modes_tieable_p

struct gcc_target targetm = TARGET_INITIALIZER;

namespace {

const pass_data pass_data_visium_reorg =
{
  RTL_PASS, /* type */
  "mach2", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_visium_reorg : public rtl_opt_pass
{
public:
  pass_visium_reorg(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_visium_reorg, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      return visium_reorg ();
    }

}; // class pass_work_around_errata

} // anon namespace

rtl_opt_pass *
make_pass_visium_reorg (gcc::context *ctxt)
{
  return new pass_visium_reorg (ctxt);
}

/* Options override for Visium.  */

static void
visium_option_override (void)
{
  if (flag_pic == 1)
    warning (OPT_fpic, "-fpic is not supported");
  if (flag_pic == 2)
    warning (OPT_fPIC, "-fPIC is not supported");

  /* MCM is the default in the GR5/GR6 era.  */
  target_flags |= MASK_MCM;

  /* FPU is the default with MCM, but don't override an explicit option.  */
  if ((target_flags_explicit & MASK_FPU) == 0)
    target_flags |= MASK_FPU;

  /* The supervisor mode is the default.  */
  if ((target_flags_explicit & MASK_SV_MODE) == 0)
    target_flags |= MASK_SV_MODE;

  /* The GR6 has the Block Move Instructions and an IEEE-compliant FPU.  */
  if (visium_cpu_and_features == PROCESSOR_GR6)
    {
      target_flags |= MASK_BMI;
      if (target_flags & MASK_FPU)
	target_flags |= MASK_FPU_IEEE;
    }

  /* Set -mtune from -mcpu if not specified.  */
  if (!global_options_set.x_visium_cpu)
    visium_cpu = visium_cpu_and_features;

  /* Align functions on 256-byte (32-quadword) for GR5 and 64-byte (8-quadword)
     boundaries for GR6 so they start a new burst mode window.  */
  if (align_functions == 0)
    {
      if (visium_cpu == PROCESSOR_GR6)
	align_functions = 64;
      else
	align_functions = 256;

      /* Allow the size of compilation units to double because of inlining.
	 In practice the global size of the object code is hardly affected
	 because the additional instructions will take up the padding.  */
      maybe_set_param_value (PARAM_INLINE_UNIT_GROWTH, 100,
			     global_options.x_param_values,
			     global_options_set.x_param_values);
    }

  /* Likewise for loops.  */
  if (align_loops == 0)
    {
      if (visium_cpu == PROCESSOR_GR6)
	align_loops = 64;
      else
	{
	  align_loops = 256;
	  /* But not if they are too far away from a 256-byte boundary.  */
	  align_loops_max_skip = 31;
	}
    }

  /* Align all jumps on quadword boundaries for the burst mode, and even
     on 8-quadword boundaries for GR6 so they start a new window.  */
  if (align_jumps == 0)
    {
      if (visium_cpu == PROCESSOR_GR6)
	align_jumps = 64;
      else
	align_jumps = 8;
    }

  /* We register a machine-specific pass.  This pass must be scheduled as
     late as possible so that we have the (essentially) final form of the
     insn stream to work on.  Registering the pass must be done at start up.
     It's convenient to do it here.  */
  opt_pass *visium_reorg_pass = make_pass_visium_reorg (g);
  struct register_pass_info insert_pass_visium_reorg =
    {
      visium_reorg_pass,		/* pass */
      "dbr",				/* reference_pass_name */
      1,				/* ref_pass_instance_number */
      PASS_POS_INSERT_AFTER		/* po_op */
    };
  register_pass (&insert_pass_visium_reorg);
}

/* Register the Visium-specific libfuncs with the middle-end.  */

static void
visium_init_libfuncs (void)
{
  if (!TARGET_BMI)
    long_int_memcpy_libfunc = init_one_libfunc ("__long_int_memcpy");
  wrd_memcpy_libfunc = init_one_libfunc ("__wrd_memcpy");
  byt_memcpy_libfunc = init_one_libfunc ("__byt_memcpy");

  long_int_memset_libfunc = init_one_libfunc ("__long_int_memset");
  wrd_memset_libfunc = init_one_libfunc ("__wrd_memset");
  byt_memset_libfunc = init_one_libfunc ("__byt_memset");

  set_trampoline_parity_libfunc = init_one_libfunc ("__set_trampoline_parity");
}

/* Return the number of instructions that can issue on the same cycle.  */

static int
visium_issue_rate (void)
{
  switch (visium_cpu)
    {
    case PROCESSOR_GR5:
      return 1;

    case PROCESSOR_GR6:
      return 2;

    default:
      gcc_unreachable ();
    }
}

/* Return the adjusted PRIORITY of INSN.  */

static int
visium_adjust_priority (rtx_insn *insn, int priority)
{
  /* On the GR5, we slightly increase the priority of writes in order to avoid
     scheduling a read on the next cycle.  This is necessary in addition to the
     associated insn reservation because there are no data dependencies.
     We also slightly increase the priority of reads from ROM in order to group
     them as much as possible.  These reads are a bit problematic because they
     conflict with the instruction fetches, i.e. the data and instruction buses
     tread on each other's toes when they are executed.  */
  if (visium_cpu == PROCESSOR_GR5
      && reload_completed
      && INSN_P (insn)
      && recog_memoized (insn) >= 0)
    {
      enum attr_type attr_type = get_attr_type (insn);
      if (attr_type == TYPE_REG_MEM
	  || (attr_type == TYPE_MEM_REG
	      && MEM_READONLY_P (SET_SRC (PATTERN (insn)))))
	return priority + 1;
    }

  return priority;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK of INSN on DEP_INSN.  COST is the current cost.  */

static int
visium_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn, int cost,
		    unsigned int)
{
  enum attr_type attr_type;

  /* Don't adjust costs for true dependencies as they are described with
     bypasses.  But we make an exception for the first scheduling pass to
     help the subsequent postreload compare elimination pass.  */
  if (dep_type == REG_DEP_TRUE)
    {
      if (!reload_completed
	  && recog_memoized (insn) >= 0
	  && get_attr_type (insn) == TYPE_CMP)
	{
	  rtx pat = PATTERN (insn);
	  gcc_assert (GET_CODE (pat) == SET);
	  rtx src = SET_SRC (pat);

	  /* Only the branches can be modified by the postreload compare
	     elimination pass, not the cstores because they accept only
	     unsigned comparison operators and they are eliminated if
	     one of the operands is zero.  */
	  if (GET_CODE (src) == IF_THEN_ELSE
	      && XEXP (XEXP (src, 0), 1) == const0_rtx
	      && recog_memoized (dep_insn) >= 0)
	    {
	      enum attr_type dep_attr_type = get_attr_type (dep_insn);

	      /* The logical instructions use CCmode and thus work with any
		 comparison operator, whereas the arithmetic instructions use
		 CCNZmode and thus work with only a small subset.  */
	      if (dep_attr_type == TYPE_LOGIC
		  || (dep_attr_type == TYPE_ARITH
		      && visium_nz_comparison_operator (XEXP (src, 0),
							GET_MODE
							(XEXP (src, 0)))))
		return 0;
	    }
	}

      return cost;
    }

  if (recog_memoized (insn) < 0)
    return 0;

  attr_type = get_attr_type (insn);

  /* Anti dependency: DEP_INSN reads a register that INSN writes some
     cycles later.  */
  if (dep_type == REG_DEP_ANTI)
    {
      /* On the GR5, the latency of FP instructions needs to be taken into
	 account for every dependency involving a write.  */
      if (attr_type == TYPE_REG_FP && visium_cpu == PROCESSOR_GR5)
	{
	  /* INSN is FLOAD. */
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);

	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally. Return 0 for now. */
	    return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_SRC (dep_pat)))
	    {
	      if (recog_memoized (dep_insn) < 0)
		return 0;

	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FDIV:
		case TYPE_FSQRT:
		case TYPE_FTOI:
		case TYPE_ITOF:
		case TYPE_FP:
		case TYPE_FMOVE:
		  /* A fload can't be issued until a preceding arithmetic
		     operation has finished if the target of the fload is
		     any of the sources (or destination) of the arithmetic
		     operation. Note that the latency may be (much)
		     greater than this if the preceding instruction
		     concerned is in a queue. */
		  return insn_default_latency (dep_insn);

		default:
		  return 0;
		}
	    }
	}

      /* On the GR6, we try to make sure that the link register is restored
	 sufficiently ahead of the return as to yield a correct prediction
	 from the branch predictor.  By default there is no true dependency
	 but an anti dependency between them, so we simply reuse it.  */
      else if (attr_type == TYPE_RET && visium_cpu == PROCESSOR_GR6)
	{
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (dep_pat) == SET
	      && REG_P (SET_DEST (dep_pat))
	      && REGNO (SET_DEST (dep_pat)) == LINK_REGNUM)
	    return 8;
	}

      /* For other anti dependencies, the cost is 0. */
      return 0;
    }

  /* Output dependency: DEP_INSN writes a register that INSN writes some
     cycles later.  */
  else if (dep_type == REG_DEP_OUTPUT)
    {
      /* On the GR5, the latency of FP instructions needs to be taken into
	 account for every dependency involving a write.  */
      if (attr_type == TYPE_REG_FP && visium_cpu == PROCESSOR_GR5)
	{
	  /* INSN is FLOAD. */
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);

	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally. Return 0 for now. */
	    return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_DEST (dep_pat)))
	    {
	      if (recog_memoized (dep_insn) < 0)
		return 0;

	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FDIV:
		case TYPE_FSQRT:
		case TYPE_FTOI:
		case TYPE_ITOF:
		case TYPE_FP:
		case TYPE_FMOVE:
		  /* A fload can't be issued until a preceding arithmetic
		     operation has finished if the target of the fload is
		     the destination of the arithmetic operation. Note that
		     the latency may be (much) greater than this if the
		     preceding instruction concerned is in a queue. */
		  return insn_default_latency (dep_insn);

		default:
		  return 0;
		}
	    }
	}

      /* For other output dependencies, the cost is 0. */
      return 0;
    }

  return 0;
}

/* Handle an "interrupt_handler" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
visium_handle_interrupt_attr (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED,
			      bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  else if (!TARGET_SV_MODE)
    {
      error ("an interrupt handler cannot be compiled with -muser-mode");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Return non-zero if the current function is an interrupt function.  */

int
visium_interrupt_function_p (void)
{
  return
    lookup_attribute ("interrupt",
		      DECL_ATTRIBUTES (current_function_decl)) != NULL_TREE;
}

/* Conditionally modify the settings of the register file.  */

static void
visium_conditional_register_usage (void)
{
  /* If the supervisor mode is disabled, mask some general registers.  */
  if (!TARGET_SV_MODE)
    {
      if (visium_cpu_and_features == PROCESSOR_GR5)
	{
	  fixed_regs[24] = call_used_regs[24] = 1;
	  fixed_regs[25] = call_used_regs[25] = 1;
	  fixed_regs[26] = call_used_regs[26] = 1;
	  fixed_regs[27] = call_used_regs[27] = 1;
	  fixed_regs[28] = call_used_regs[28] = 1;
	  call_really_used_regs[24] = 0;
	  call_really_used_regs[25] = 0;
	  call_really_used_regs[26] = 0;
	  call_really_used_regs[27] = 0;
	  call_really_used_regs[28] = 0;
	}

      fixed_regs[31] = call_used_regs[31] = 1;
      call_really_used_regs[31] = 0;

      /* We also need to change the long-branch register.  */
      if (visium_cpu_and_features == PROCESSOR_GR5)
	long_branch_regnum = 20;
      else
	long_branch_regnum = 28;
    }

  /* If the FPU is disabled, mask the FP registers.  */
  if (!TARGET_FPU)
    {
      for (int i = FP_FIRST_REGNUM; i <= FP_LAST_REGNUM; i++)
	{
	  fixed_regs[i] = call_used_regs[i] = 1;
	  call_really_used_regs[i] = 0;
	}
    }
}

/* Prepend to CLOBBERS hard registers that are automatically clobbered for
   an asm   We do this for the FLAGS to maintain source compatibility with
   the original cc0-based compiler.  */

static rtx_insn *
visium_md_asm_adjust (vec<rtx> &/*outputs*/, vec<rtx> &/*inputs*/,
		      vec<const char *> &/*constraints*/,
		      vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs)
{
  clobbers.safe_push (gen_rtx_REG (CCmode, FLAGS_REGNUM));
  SET_HARD_REG_BIT (clobbered_regs, FLAGS_REGNUM);
  return NULL;
}

/* Return true if X is a legitimate constant for a MODE immediate operand.
   X is guaranteed to satisfy the CONSTANT_P predicate.  */

static bool
visium_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED,
			      rtx x ATTRIBUTE_UNUSED)
{
  return true;
}

/* Compute the alignment for a variable.  The alignment of an aggregate is
   set to be at least that of a scalar less than or equal to it in size.  */

unsigned int
visium_data_alignment (tree type, unsigned int align)
{
  if (AGGREGATE_TYPE_P (type)
      && TYPE_SIZE (type)
      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST && align < 32)
    {
      if (TREE_INT_CST_LOW (TYPE_SIZE (type)) >= 32)
	return 32;

      if (TREE_INT_CST_LOW (TYPE_SIZE (type)) >= 16 && align < 16)
	return 16;
    }

  return align;
}

/* Helper function for HARD_REGNO_RENAME_OK (FROM, TO).  Return non-zero if
   it is OK to rename a hard register FROM to another hard register TO.  */

int
visium_hard_regno_rename_ok (unsigned int from ATTRIBUTE_UNUSED,
			     unsigned int to)
{
  /* If the function doesn't save LR, then the long-branch register will be
     used for long branches so we need to know whether it is live before the
     frame layout is computed.  */
  if (!current_function_saves_lr () && to == long_branch_regnum)
    return 0;

  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be call-clobbered.  */
  if (crtl->is_leaf
      && !df_regs_ever_live_p (to)
      && visium_interrupt_function_p ())
    return 0;

  return 1;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.

   Modes with sizes which cross from the one register class to the
   other cannot be allowed. Only single floats are allowed in the
   floating point registers, and only fixed point values in the EAM
   registers. */

static bool
visium_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (GP_REGISTER_P (regno))
    return GP_REGISTER_P (regno + HARD_REGNO_NREGS (regno, mode) - 1);

  if (FP_REGISTER_P (regno))
    return mode == SFmode || (mode == SImode && TARGET_FPU_IEEE);

  return (GET_MODE_CLASS (mode) == MODE_INT
	  && HARD_REGNO_NREGS (regno, mode) == 1);
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
visium_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  return (GET_MODE_CLASS (mode1) == MODE_INT
	  && GET_MODE_CLASS (mode2) == MODE_INT);
}

/* Return true if it is ok to do sibling call optimization for the specified
   call expression EXP.  DECL will be the called function, or NULL if this
   is an indirect call.  */

static bool
visium_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
				tree exp ATTRIBUTE_UNUSED)
{
  return !visium_interrupt_function_p ();
}

/* Prepare operands for a move define_expand in MODE.  */

void
prepare_move_operands (rtx *operands, machine_mode mode)
{
  /* If the output is not a register, the input must be.  */
  if (GET_CODE (operands[0]) == MEM && !reg_or_0_operand (operands[1], mode))
    operands[1] = force_reg (mode, operands[1]);
}

/* Return true if the operands are valid for a simple move insn.  */

bool
ok_for_simple_move_operands (rtx *operands, machine_mode mode)
{
  /* One of the operands must be a register.  */
  if (!register_operand (operands[0], mode)
      && !reg_or_0_operand (operands[1], mode))
    return false;

  /* Once the flags are exposed, no simple moves between integer registers.  */
  if (visium_flags_exposed
      && gpc_reg_operand (operands[0], mode)
      && gpc_reg_operand (operands[1], mode))
    return false;

 return true;
}

/* Return true if the operands are valid for a simple move strict insn.  */

bool
ok_for_simple_move_strict_operands (rtx *operands, machine_mode mode)
{
  /* Once the flags are exposed, no simple moves between integer registers.
     Note that, in QImode only, a zero source counts as an integer register
     since it will be emitted as r0.  */
  if (visium_flags_exposed
      && gpc_reg_operand (operands[0], mode)
      && (gpc_reg_operand (operands[1], mode)
	  || (mode == QImode && operands[1] == const0_rtx)))
    return false;

 return true;
}

/* Return true if the operands are valid for a simple arithmetic or logical
   insn.  */

bool
ok_for_simple_arith_logic_operands (rtx *, machine_mode)
{
  /* Once the flags are exposed, no simple arithmetic or logical operations
     between integer registers.  */
  return !visium_flags_exposed;
}

/* Return non-zero if a branch or call instruction will be emitting a nop
   into its delay slot.  */

int
empty_delay_slot (rtx_insn *insn)
{
  rtx seq;

  /* If no previous instruction (should not happen), return true.  */
  if (PREV_INSN (insn) == NULL)
    return 1;

  seq = NEXT_INSN (PREV_INSN (insn));
  if (GET_CODE (PATTERN (seq)) == SEQUENCE)
    return 0;

  return 1;
}

/* Wrapper around single_set which returns the second SET of a pair if the
   first SET is to the flags register.  */

static rtx
single_set_and_flags (rtx_insn *insn)
{
  if (multiple_sets (insn))
    {
      rtx pat = PATTERN (insn);
      if (XVECLEN (pat, 0) == 2
	  && GET_CODE (XVECEXP (pat, 0, 0)) == SET
	  && REG_P (SET_DEST (XVECEXP (pat, 0, 0)))
	  && REGNO (SET_DEST (XVECEXP (pat, 0, 0))) == FLAGS_REGNUM)
	return XVECEXP (pat, 0, 1);
    }

  return single_set (insn);
}

/* This is called with OUT_INSN an instruction setting a (base) register
   and IN_INSN a read or a write.  Return 1 if these instructions together
   constitute a pipeline hazard.

   On the original architecture, a pipeline data hazard occurs when the Dest
   of one instruction becomes the SrcA for an immediately following READ or
   WRITE instruction with a non-zero index (indexing occurs at the decode
   stage and so a NOP must be inserted in-between for this to work).

   An example is:

	move.l  r2,r1
	read.l  r4,10(r2)

   On the MCM, the non-zero index condition is lifted but the hazard is
   patched up by the hardware through the injection of wait states:

        move.l  r2,r1
        read.l  r4,(r2)

   We nevertheless try to schedule instructions around this.  */

int
gr5_hazard_bypass_p (rtx_insn *out_insn, rtx_insn *in_insn)
{
  rtx out_set, in_set, dest, memexpr;
  unsigned int out_reg, in_reg;

  /* A CALL is storage register class, but the link register is of no
     interest here. */
  if (GET_CODE (out_insn) == CALL_INSN)
    return 0;

  out_set = single_set_and_flags (out_insn);
  dest = SET_DEST (out_set);

  /* Should be no stall/hazard if OUT_INSN is MEM := ???.  This only
     occurs prior to reload. */
  if (GET_CODE (dest) == MEM)
    return 0;

  if (GET_CODE (dest) == STRICT_LOW_PART)
    dest = XEXP (dest, 0);
  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);
  out_reg = REGNO (dest);

  in_set = single_set_and_flags (in_insn);

  /* If IN_INSN is MEM := MEM, it's the source that counts. */
  if (GET_CODE (SET_SRC (in_set)) == MEM)
    memexpr = XEXP (SET_SRC (in_set), 0);
  else
    memexpr = XEXP (SET_DEST (in_set), 0);

  if (GET_CODE (memexpr) == PLUS)
    {
      memexpr = XEXP (memexpr, 0);
      if (GET_CODE (memexpr) == SUBREG)
	in_reg = REGNO (SUBREG_REG (memexpr));
      else
	in_reg = REGNO (memexpr);

      if (in_reg == out_reg)
	return 1;
    }
  else if (TARGET_MCM)
    {
      if (GET_CODE (memexpr) == STRICT_LOW_PART)
	memexpr = XEXP (memexpr, 0);
      if (GET_CODE (memexpr) == SUBREG)
	memexpr = SUBREG_REG (memexpr);
      in_reg = REGNO (memexpr);

      if (in_reg == out_reg)
	return 1;
    }

  return 0;
}

/* Return true if INSN is an empty asm instruction.  */

static bool
empty_asm_p (rtx insn)
{
  rtx body = PATTERN (insn);
  const char *templ;

  if (GET_CODE (body) == ASM_INPUT)
    templ = XSTR (body, 0);
  else if (asm_noperands (body) >= 0)
    templ = decode_asm_operands (body, NULL, NULL, NULL, NULL, NULL);
  else
    templ = NULL;

  return (templ && templ[0] == '\0');
}

/* Insert a NOP immediately before INSN wherever there is a pipeline hazard.
   LAST_REG records the register set in the last insn and LAST_INSN_CALL
   records whether the last insn was a call insn.  */

static void
gr5_avoid_hazard (rtx_insn *insn, unsigned int *last_reg, bool *last_insn_call)
{
  unsigned int dest_reg = 0;
  rtx set;

  switch (GET_CODE (insn))
    {
    case CALL_INSN:
      *last_reg = 0;
      *last_insn_call = true;
      return;

    case JUMP_INSN:
      /* If this is an empty asm, just skip it.  */
      if (!empty_asm_p (insn))
	{
	  *last_reg = 0;
	  *last_insn_call = false;
	}
      return;

    case INSN:
      /* If this is an empty asm, just skip it.  */
      if (empty_asm_p (insn))
	return;
      break;

    default:
      return;
    }

  set = single_set_and_flags (insn);
  if (set != NULL_RTX)
    {
      rtx dest = SET_DEST (set);
      const bool double_p = GET_MODE_SIZE (GET_MODE (dest)) > UNITS_PER_WORD;
      rtx memrtx = NULL;

      if (GET_CODE (SET_SRC (set)) == MEM)
	{
	  memrtx = XEXP (SET_SRC (set), 0);
	  if (GET_CODE (dest) == STRICT_LOW_PART)
	    dest = XEXP (dest, 0);
	  if (REG_P (dest))
	    dest_reg = REGNO (dest);

	  /* If this is a DI or DF mode memory to register
	     copy, then if rd = rs we get

	     rs + 1 := 1[rs]
	     rs     :=  [rs]

	     otherwise the order is

	     rd     :=  [rs]
	     rd + 1 := 1[rs] */

	  if (double_p)
	    {
	      unsigned int base_reg;

	      if (GET_CODE (memrtx) == PLUS)
		base_reg = REGNO (XEXP (memrtx, 0));
	      else
		base_reg = REGNO (memrtx);

	      if (dest_reg != base_reg)
		dest_reg++;
	    }
	}

      else if (GET_CODE (dest) == MEM)
	memrtx = XEXP (dest, 0);

      else if (GET_MODE_CLASS (GET_MODE (dest)) != MODE_CC)
	{
	  if (GET_CODE (dest) == STRICT_LOW_PART
	      ||GET_CODE (dest) == ZERO_EXTRACT)
	    dest = XEXP (dest, 0);
	  dest_reg = REGNO (dest);

	  if (GET_CODE (SET_SRC (set)) == REG)
	    {
	      unsigned int srcreg = REGNO (SET_SRC (set));

	      /* Check for rs := rs, which will be deleted.  */
	      if (srcreg == dest_reg)
		return;

	      /* In the case of a DI or DF mode move from register to
	         register there is overlap if rd = rs + 1 in which case
	         the order of the copies is reversed :

	         rd + 1 := rs + 1;
	         rd     := rs   */

	      if (double_p && dest_reg != srcreg + 1)
		dest_reg++;
	    }
	}

      /* If this is the delay slot of a call insn, any register it sets
         is not relevant.  */
      if (*last_insn_call)
	dest_reg = 0;

      /* If the previous insn sets the value of a register, and this insn
	 uses a base register, check for the pipeline hazard where it is
	 the same register in each case.  */
      if (*last_reg != 0 && memrtx != NULL_RTX)
	{
	  unsigned int reg = 0;

	  /* Check for an index (original architecture).  */
	  if (GET_CODE (memrtx) == PLUS)
	    reg = REGNO (XEXP (memrtx, 0));

	  /* Check for an MCM target or rs := [rs], in DI or DF mode.  */
	  else if (TARGET_MCM || (double_p && REGNO (memrtx) == dest_reg))
	    reg = REGNO (memrtx);

	  /* Remove any pipeline hazard by inserting a NOP.  */
	  if (reg == *last_reg)
	    {
	      if (dump_file)
	        fprintf (dump_file,
			 "inserting nop before insn %d\n", INSN_UID (insn));
	      emit_insn_after (gen_hazard_nop (), prev_active_insn (insn));
	      emit_insn_after (gen_blockage (), insn);
	    }
	}

      *last_reg = dest_reg;
    }

  *last_insn_call = false;
}

/* Go through the instruction stream and insert nops where necessary to avoid
   pipeline hazards.  There are two cases:

     1. On the original architecture, it is invalid to set the value of a
	(base) register and then use it in an address with a non-zero index
	in the next instruction.

     2. On the MCM, setting the value of a (base) register and then using
	it in address (including with zero index) in the next instruction
	will result in a pipeline stall of 3 cycles.  */

static void
gr5_hazard_avoidance (void)
{
  unsigned int last_reg = 0;
  bool last_insn_call = false;
  rtx_insn *insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	rtx pat = PATTERN (insn);

	if (GET_CODE (pat) == SEQUENCE)
	  {
	    for (int i = 0; i < XVECLEN (pat, 0); i++)
	      gr5_avoid_hazard (as_a <rtx_insn *> (XVECEXP (pat, 0, i)),
				&last_reg, &last_insn_call);
	  }

	else if (GET_CODE (insn) == CALL_INSN)
	  {
	    /* This call is going to get a nop in its delay slot.  */
	    last_reg = 0;
	    last_insn_call = false;
	  }

	else
	  gr5_avoid_hazard (insn, &last_reg, &last_insn_call);
      }

    else if (GET_CODE (insn) == BARRIER)
      last_reg = 0;
}

/* Perform a target-specific pass over the instruction stream.  The compiler
   will run it at all optimization levels, just after the point at which it
   normally does delayed-branch scheduling.  */

static unsigned int
visium_reorg (void)
{
  if (visium_cpu == PROCESSOR_GR5)
    gr5_hazard_avoidance ();

  return 0;
}
/* Return true if an argument must be passed by indirect reference.  */

static bool
visium_pass_by_reference (cumulative_args_t ca ATTRIBUTE_UNUSED,
			  machine_mode mode ATTRIBUTE_UNUSED,
			  const_tree type,
			  bool named ATTRIBUTE_UNUSED)
{
  return type && (AGGREGATE_TYPE_P (type) || TREE_CODE (type) == VECTOR_TYPE);
}

/* Define how arguments are passed.

   A range of general registers and floating registers is available
   for passing arguments.  When the class of registers which an
   argument would normally use is exhausted, that argument, is passed
   in the overflow region of the stack.  No argument is split between
   registers and stack.

   Arguments of type float or _Complex float go in FP registers if FP
   hardware is available.  If there is no FP hardware, arguments of
   type float go in general registers.  All other arguments are passed
   in general registers.  */

static rtx
visium_function_arg (cumulative_args_t pcum_v, machine_mode mode,
		     const_tree type ATTRIBUTE_UNUSED,
		     bool named ATTRIBUTE_UNUSED)
{
  int size;
  CUMULATIVE_ARGS *ca = get_cumulative_args (pcum_v);

  size = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  if (mode == VOIDmode)
    return GEN_INT (0);

  /* Scalar or complex single precision floating point arguments are returned
     in floating registers.  */
  if (TARGET_FPU
      && ((GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE)
	  || (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
	      && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE * 2)))
    {
      if (ca->frcount + size <= MAX_ARGS_IN_FP_REGISTERS)
	return gen_rtx_REG (mode, FP_ARG_FIRST + ca->frcount);
      else
	return NULL_RTX;
    }

  if (ca->grcount + size <= MAX_ARGS_IN_GP_REGISTERS)
    return gen_rtx_REG (mode, ca->grcount + GP_ARG_FIRST);

  return NULL_RTX;
}

/* Update the summarizer variable pointed to by PCUM_V to advance past an
   argument in the argument list.  The values MODE, TYPE and NAMED describe
   that argument.  Once this is done, the variable CUM is suitable for
   analyzing the _following_ argument with visium_function_arg.  */

static void
visium_function_arg_advance (cumulative_args_t pcum_v,
			     machine_mode mode,
			     const_tree type ATTRIBUTE_UNUSED,
			     bool named)
{
  int size = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  int stack_size = 0;
  CUMULATIVE_ARGS *ca = get_cumulative_args (pcum_v);

  /* Scalar or complex single precision floating point arguments are returned
     in floating registers.  */
  if (TARGET_FPU
      && ((GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE)
	  || (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
	      && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE * 2)))
    {
      if (ca->frcount + size <= MAX_ARGS_IN_FP_REGISTERS)
	ca->frcount += size;
      else
	{
	  stack_size = size;
	  ca->frcount = MAX_ARGS_IN_FP_REGISTERS;
	}
    }
  else
    {
      /* Everything else goes in a general register, if enough are
	 available.  */
      if (ca->grcount + size <= MAX_ARGS_IN_GP_REGISTERS)
	ca->grcount += size;
      else
	{
	  stack_size = size;
	  ca->grcount = MAX_ARGS_IN_GP_REGISTERS;
	}
    }

  if (named)
    ca->stack_words += stack_size;
}

/* Specify whether to return the return value in memory.  */

static bool
visium_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  return (AGGREGATE_TYPE_P (type) || TREE_CODE (type) == VECTOR_TYPE);
}

/* Define how scalar values are returned.  */

static rtx
visium_function_value_1 (machine_mode mode)
{
  /* Scalar or complex single precision floating point values
     are returned in floating register f1.  */
  if (TARGET_FPU
      && ((GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE)
	  || (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
	      && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE * 2)))
    return gen_rtx_REG (mode, FP_RETURN_REGNUM);

  /* All others are returned in r1.  */
  return gen_rtx_REG (mode, RETURN_REGNUM);
}

/* Return an RTX representing the place where a function returns or receives
   a value of data type RET_TYPE.  */

static rtx
visium_function_value (const_tree ret_type,
		       const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  return visium_function_value_1 (TYPE_MODE (ret_type));
}

/* Return an RTX representing the place where the library function result will
   be returned.  */

static rtx
visium_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return visium_function_value_1 (mode);
}

/* Store the anonymous register arguments into the stack so that all the
   arguments appear to have been passed consecutively on the stack.  */

static void
visium_setup_incoming_varargs (cumulative_args_t pcum_v,
			       machine_mode mode,
			       tree type,
			       int *pretend_size ATTRIBUTE_UNUSED,
			       int no_rtl)
{
  cumulative_args_t local_args_so_far;
  CUMULATIVE_ARGS local_copy;
  CUMULATIVE_ARGS *locargs;
  int gp_saved, fp_saved, size;

  /* Create an internal cumulative_args_t pointer to internally define
     storage to ensure calling TARGET_FUNCTION_ARG_ADVANCE does not
     make global changes.  */
  local_args_so_far.p = &local_copy;
  locargs = get_cumulative_args (pcum_v);

#if CHECKING_P
  local_args_so_far.magic = CUMULATIVE_ARGS_MAGIC;
#endif

  local_copy.grcount = locargs->grcount;
  local_copy.frcount = locargs->frcount;
  local_copy.stack_words = locargs->stack_words;

  /* The caller has advanced ARGS_SO_FAR up to, but not beyond, the last named
     argument.  Advance a local copy of ARGS_SO_FAR past the last "real" named
     argument, to find out how many registers are left over.  */
  TARGET_FUNCTION_ARG_ADVANCE (local_args_so_far, mode, type, 1);

  /* Find how many registers we need to save.  */
  locargs = get_cumulative_args (local_args_so_far);
  gp_saved = MAX_ARGS_IN_GP_REGISTERS - locargs->grcount;
  fp_saved = (TARGET_FPU ? MAX_ARGS_IN_FP_REGISTERS - locargs->frcount : 0);
  size = (gp_saved * UNITS_PER_WORD) + (fp_saved * UNITS_PER_HWFPVALUE);

  if (!no_rtl && size > 0)
    {
      /* To avoid negative offsets, which are not valid addressing modes on
	 the Visium, we create a base register for the pretend args.  */
      rtx ptr
	= force_reg (Pmode,
		     plus_constant (Pmode, virtual_incoming_args_rtx, -size));

      if (gp_saved > 0)
	{
	  rtx mem
	    = gen_rtx_MEM (BLKmode,
			   plus_constant (Pmode,
					  ptr,
					  fp_saved * UNITS_PER_HWFPVALUE));
	  MEM_NOTRAP_P (mem) = 1;
	  set_mem_alias_set (mem, get_varargs_alias_set ());
	  move_block_from_reg (locargs->grcount + GP_ARG_FIRST, mem, gp_saved);
	}

      if (fp_saved > 0)
	{
	  rtx mem = gen_rtx_MEM (BLKmode, ptr);
	  MEM_NOTRAP_P (mem) = 1;
	  set_mem_alias_set (mem, get_varargs_alias_set ());
	  gcc_assert (UNITS_PER_WORD == UNITS_PER_HWFPVALUE);
	  move_block_from_reg (locargs->frcount + FP_ARG_FIRST, mem, fp_saved);
	}
    }

  visium_reg_parm_save_area_size = size;
}

/* Define the `__builtin_va_list' type for the ABI.  */

static tree
visium_build_builtin_va_list (void)
{
  tree f_ovfl, f_gbase, f_fbase, f_gbytes, f_fbytes, record;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  f_ovfl = build_decl (BUILTINS_LOCATION, FIELD_DECL,
		       get_identifier ("__overflow_argptr"), ptr_type_node);
  f_gbase = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			get_identifier ("__gpr_base"), ptr_type_node);
  f_fbase = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			get_identifier ("__fpr_base"), ptr_type_node);
  f_gbytes = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			 get_identifier ("__gpr_bytes"),
			 short_unsigned_type_node);
  f_fbytes = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			 get_identifier ("__fpr_bytes"),
			 short_unsigned_type_node);

  DECL_FIELD_CONTEXT (f_ovfl) = record;
  DECL_FIELD_CONTEXT (f_gbase) = record;
  DECL_FIELD_CONTEXT (f_fbase) = record;
  DECL_FIELD_CONTEXT (f_gbytes) = record;
  DECL_FIELD_CONTEXT (f_fbytes) = record;
  TYPE_FIELDS (record) = f_ovfl;
  TREE_CHAIN (f_ovfl) = f_gbase;
  TREE_CHAIN (f_gbase) = f_fbase;
  TREE_CHAIN (f_fbase) = f_gbytes;
  TREE_CHAIN (f_gbytes) = f_fbytes;
  layout_type (record);

  return record;
}

/* Implement `va_start' for varargs and stdarg.  */

static void
visium_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  const CUMULATIVE_ARGS *ca = &crtl->args.info;
  int gp_saved = MAX_ARGS_IN_GP_REGISTERS - ca->grcount;
  int fp_saved = (TARGET_FPU ? MAX_ARGS_IN_FP_REGISTERS - ca->frcount : 0);
  int named_stack_size = ca->stack_words * UNITS_PER_WORD, offset;
  tree f_ovfl, f_gbase, f_fbase, f_gbytes, f_fbytes;
  tree ovfl, gbase, gbytes, fbase, fbytes, t;

  f_ovfl = TYPE_FIELDS (va_list_type_node);
  f_gbase = TREE_CHAIN (f_ovfl);
  f_fbase = TREE_CHAIN (f_gbase);
  f_gbytes = TREE_CHAIN (f_fbase);
  f_fbytes = TREE_CHAIN (f_gbytes);
  ovfl = build3 (COMPONENT_REF, TREE_TYPE (f_ovfl), valist, f_ovfl, NULL_TREE);
  gbase = build3 (COMPONENT_REF, TREE_TYPE (f_gbase), valist, f_gbase,
		  NULL_TREE);
  fbase = build3 (COMPONENT_REF, TREE_TYPE (f_fbase), valist, f_fbase,
		  NULL_TREE);
  gbytes = build3 (COMPONENT_REF, TREE_TYPE (f_gbytes), valist, f_gbytes,
		   NULL_TREE);
  fbytes = build3 (COMPONENT_REF, TREE_TYPE (f_fbytes), valist, f_fbytes,
		   NULL_TREE);

  /* Store the stacked vararg pointer in the OVFL member.  */
  t = make_tree (TREE_TYPE (ovfl), virtual_incoming_args_rtx);
  t = fold_build_pointer_plus_hwi (t, named_stack_size);
  t = build2 (MODIFY_EXPR, TREE_TYPE (ovfl), ovfl, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Store the base address of the GPR save area into GBASE.  */
  t = make_tree (TREE_TYPE (gbase), virtual_incoming_args_rtx);
  offset = MAX_ARGS_IN_GP_REGISTERS * UNITS_PER_WORD;
  t = fold_build_pointer_plus_hwi (t, -offset);
  t = build2 (MODIFY_EXPR, TREE_TYPE (gbase), gbase, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Store the base address of the FPR save area into FBASE.  */
  if (fp_saved)
    {
      t = make_tree (TREE_TYPE (fbase), virtual_incoming_args_rtx);
      offset = gp_saved * UNITS_PER_WORD
	         + MAX_ARGS_IN_FP_REGISTERS * UNITS_PER_HWFPVALUE;
      t = fold_build_pointer_plus_hwi (t, -offset);
      t = build2 (MODIFY_EXPR, TREE_TYPE (fbase), fbase, t);
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  /* Fill in the GBYTES member.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (gbytes), gbytes,
	      size_int (gp_saved * UNITS_PER_WORD));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Fill in the FBYTES member.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (fbytes),
	      fbytes, size_int (fp_saved * UNITS_PER_HWFPVALUE));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement `va_arg'.  */

static tree
visium_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
			gimple_seq *post_p)
{
  tree f_ovfl, f_gbase, f_fbase, f_gbytes, f_fbytes;
  tree ovfl, base, bytes;
  HOST_WIDE_INT size, rsize;
  const bool by_reference_p
    = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  const bool float_reg_arg_p
    = (TARGET_FPU && !by_reference_p
       && ((GET_MODE_CLASS (TYPE_MODE (type)) == MODE_FLOAT
	    && GET_MODE_SIZE (TYPE_MODE (type)) <= UNITS_PER_HWFPVALUE)
	   || (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_COMPLEX_FLOAT
	       && (GET_MODE_SIZE (TYPE_MODE (type))
		   <= UNITS_PER_HWFPVALUE * 2))));
  const int max_save_area_size
    = (float_reg_arg_p ? MAX_ARGS_IN_FP_REGISTERS * UNITS_PER_HWFPVALUE
       : MAX_ARGS_IN_GP_REGISTERS * UNITS_PER_WORD);
  tree t, u, offs;
  tree lab_false, lab_over, addr;
  tree ptrtype = build_pointer_type (type);

  if (by_reference_p)
    {
      t = visium_gimplify_va_arg (valist, ptrtype, pre_p, post_p);
      return build_va_arg_indirect_ref (t);
    }

  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;
  f_ovfl = TYPE_FIELDS (va_list_type_node);
  f_gbase = TREE_CHAIN (f_ovfl);
  f_fbase = TREE_CHAIN (f_gbase);
  f_gbytes = TREE_CHAIN (f_fbase);
  f_fbytes = TREE_CHAIN (f_gbytes);

  /* We maintain separate pointers and offsets for floating-point and
     general registers, but we need similar code in both cases.

     Let:

     BYTES be the number of unused bytes in the register save area.
     BASE be the base address of the register save area.
     OFFS be the current offset into the register save area. Either
     MAX_ARGS_IN_GP_REGISTERS * UNITS_PER_WORD - bytes or
     MAX_ARGS_IN_FP_REGISTERS * UNITS_PER_HWFPVALUE - bytes
     depending upon whether the argument is in general or floating
     registers.
     ADDR_RTX be the address of the argument.
     RSIZE be the size in bytes of the argument.
     OVFL be the pointer to the stack overflow area.

     The code we want is:

     1: if (bytes >= rsize)
     2:   {
     3:     addr_rtx = base + offs;
     4:     bytes -= rsize;
     5:   }
     6: else
     7:   {
     8:     bytes = 0;
     9:     addr_rtx = ovfl;
     10:    ovfl += rsize;
     11:  }

   */

  addr = create_tmp_var (ptr_type_node, "addr");
  lab_false = create_artificial_label (UNKNOWN_LOCATION);
  lab_over = create_artificial_label (UNKNOWN_LOCATION);
  if (float_reg_arg_p)
    bytes = build3 (COMPONENT_REF, TREE_TYPE (f_fbytes), unshare_expr (valist),
		    f_fbytes, NULL_TREE);
  else
    bytes = build3 (COMPONENT_REF, TREE_TYPE (f_gbytes), unshare_expr (valist),
		    f_gbytes, NULL_TREE);

  /* [1] Emit code to branch if bytes < rsize.  */
  t = fold_convert (TREE_TYPE (bytes), size_int (rsize));
  t = build2 (LT_EXPR, boolean_type_node, bytes, t);
  u = build1 (GOTO_EXPR, void_type_node, lab_false);
  t = build3 (COND_EXPR, void_type_node, t, u, NULL_TREE);
  gimplify_and_add (t, pre_p);

  /* [3] Emit code for: addr_rtx = base + offs, where
     offs = max_save_area_size - bytes.  */
  t = fold_convert (sizetype, bytes);
  offs = build2 (MINUS_EXPR, sizetype, size_int (max_save_area_size), t);
  if (float_reg_arg_p)
    base = build3 (COMPONENT_REF, TREE_TYPE (f_fbase), valist, f_fbase,
		   NULL_TREE);
  else
    base = build3 (COMPONENT_REF, TREE_TYPE (f_gbase), valist, f_gbase,
		   NULL_TREE);

  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (base), base, offs);
  t = build2 (MODIFY_EXPR, void_type_node, addr, t);
  gimplify_and_add (t, pre_p);

  /* [4] Emit code for: bytes -= rsize. */
  t = fold_convert (TREE_TYPE (bytes), size_int (rsize));
  t = build2 (MINUS_EXPR, TREE_TYPE (bytes), bytes, t);
  t = build2 (MODIFY_EXPR, TREE_TYPE (bytes), bytes, t);
  gimplify_and_add (t, pre_p);

  /* [6] Emit code to branch over the else clause, then the label.  */
  t = build1 (GOTO_EXPR, void_type_node, lab_over);
  gimplify_and_add (t, pre_p);
  t = build1 (LABEL_EXPR, void_type_node, lab_false);
  gimplify_and_add (t, pre_p);

  /* [8] Emit code for: bytes = 0. */
  t = fold_convert (TREE_TYPE (bytes), size_int (0));
  t = build2 (MODIFY_EXPR, TREE_TYPE (bytes), unshare_expr (bytes), t);
  gimplify_and_add (t, pre_p);

  /* [9] Emit code for: addr_rtx = ovfl. */
  ovfl = build3 (COMPONENT_REF, TREE_TYPE (f_ovfl), valist, f_ovfl, NULL_TREE);
  t = build2 (MODIFY_EXPR, void_type_node, addr, ovfl);
  gimplify_and_add (t, pre_p);

  /* [10] Emit code for: ovfl += rsize. */
  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (ovfl), ovfl, size_int (rsize));
  t = build2 (MODIFY_EXPR, TREE_TYPE (ovfl), unshare_expr (ovfl), t);
  gimplify_and_add (t, pre_p);
  t = build1 (LABEL_EXPR, void_type_node, lab_over);
  gimplify_and_add (t, pre_p);

  /* Emit a big-endian correction if size < UNITS_PER_WORD.  */
  if (size < UNITS_PER_WORD)
    {
      t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (addr), addr,
		  size_int (UNITS_PER_WORD - size));
      t = build2 (MODIFY_EXPR, void_type_node, addr, t);
      gimplify_and_add (t, pre_p);
    }

  addr = fold_convert (ptrtype, addr);

  return build_va_arg_indirect_ref (addr);
}

/* Return true if OP is an offset suitable for use as a displacement in the
   address of a memory access in mode MODE.  */

static bool
rtx_ok_for_offset_p (machine_mode mode, rtx op)
{
  if (!CONST_INT_P (op) || INTVAL (op) < 0)
    return false;

  switch (mode)
    {
    case E_QImode:
      return INTVAL (op) <= 31;

    case E_HImode:
      return (INTVAL (op) % 2) == 0 && INTVAL (op) < 63;

    case E_SImode:
    case E_SFmode:
      return (INTVAL (op) % 4) == 0 && INTVAL (op) < 127;

    case E_DImode:
    case E_DFmode:
      return (INTVAL (op) % 4) == 0 && INTVAL (op) < 123;

    default:
      return false;
    }
}

/* Return whether X is a legitimate memory address for a memory operand
   of mode MODE.

   Legitimate addresses are defined in two variants: a strict variant
   and a non-strict one.  The STRICT parameter chooses which variant
   is desired by the caller.

   The strict variant is used in the reload pass.  It must be defined
   so that any pseudo-register that has not been allocated a hard
   register is considered a memory reference.  This is because in
   contexts where some kind of register is required, a
   pseudo-register with no hard register must be rejected.  For
   non-hard registers, the strict variant should look up the
   `reg_renumber' array; it should then proceed using the hard
   register number in the array, or treat the pseudo as a memory
   reference if the array holds `-1'.

   The non-strict variant is used in other passes.  It must be
   defined to accept all pseudo-registers in every context where some
   kind of register is required.  */

static bool
visium_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  rtx base;
  unsigned int regno;

  /* If X is base+disp, check that we have an appropriate offset.  */
  if (GET_CODE (x) == PLUS)
    {
      if (!rtx_ok_for_offset_p (mode, XEXP (x, 1)))
	return false;
      base = XEXP (x, 0);
    }
  else
    base = x;

  /* Now check the base: it must be either a register or a subreg thereof.  */
  if (GET_CODE (base) == SUBREG)
    base = SUBREG_REG (base);
  if (!REG_P (base))
    return false;

  regno = REGNO (base);

  /* For the strict variant, the register must be REGNO_OK_FOR_BASE_P.  */
  if (strict)
    return REGNO_OK_FOR_BASE_P (regno);

  /* For the non-strict variant, the register may also be a pseudo.  */
  return BASE_REGISTER_P (regno) || regno >= FIRST_PSEUDO_REGISTER;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For Visium

	memory (reg + <out of range int>)

   is transformed to

	base_int = <out of range int> & ~mask
        ptr_reg = reg + base_int
        memory (ptr_reg + <out of range int> - base_int)

   Thus ptr_reg is a base register for a range of addresses,
   which should help CSE.

   For a 1 byte reference mask is 0x1f
   for a 2 byte reference mask is 0x3f
   For a 4 byte reference mask is 0x7f

   This reflects the indexing range of the processor.

   For a > 4 byte reference the mask is 0x7f provided all of the words
   can be accessed with the base address obtained.  Otherwise a mask
   of 0x3f is used.

   On rare occasions an unaligned base register value with an
   unaligned offset is generated. Unaligned offsets are left alone for
   this reason. */

static rtx
visium_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			   machine_mode mode)
{
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && GET_CODE (XEXP (x, 0)) == REG && mode != BLKmode)
    {
      int offset = INTVAL (XEXP (x, 1));
      int size = GET_MODE_SIZE (mode);
      int mask = (size == 1 ? 0x1f : (size == 2 ? 0x3f : 0x7f));
      int mask1 = (size == 1 ? 0 : (size == 2 ? 1 : 3));
      int offset_base = offset & ~mask;

      /* Check that all of the words can be accessed.  */
      if (4 < size && 0x80 < size + offset - offset_base)
	offset_base = offset & ~0x3f;
      if (offset_base != 0 && offset_base != offset && (offset & mask1) == 0)
	{
	  rtx ptr_reg = force_reg (Pmode,
				   gen_rtx_PLUS (Pmode,
						 XEXP (x, 0),
						 GEN_INT (offset_base)));

	  return plus_constant (Pmode, ptr_reg, offset - offset_base);
	}
    }

  return x;
}

/* Perform a similar function to visium_legitimize_address, but this time
   for reload.  Generating new registers is not an option here.  Parts
   that need reloading are indicated by calling push_reload.  */

rtx
visium_legitimize_reload_address (rtx x, machine_mode mode, int opnum,
				  int type, int ind ATTRIBUTE_UNUSED)
{
  rtx newrtx, tem = NULL_RTX;

  if (mode == BLKmode)
    return NULL_RTX;

  if (optimize && GET_CODE (x) == PLUS)
    tem = simplify_binary_operation (PLUS, GET_MODE (x), XEXP (x, 0),
				     XEXP (x, 1));

  newrtx = tem ? tem : x;
  if (GET_CODE (newrtx) == PLUS
      && GET_CODE (XEXP (newrtx, 1)) == CONST_INT
      && GET_CODE (XEXP (newrtx, 0)) == REG
      && BASE_REGISTER_P (REGNO (XEXP (newrtx, 0))))
    {
      int offset = INTVAL (XEXP (newrtx, 1));
      int size = GET_MODE_SIZE (mode);
      int mask = (size == 1 ? 0x1f : (size == 2 ? 0x3f : 0x7f));
      int mask1 = (size == 1 ? 0 : (size == 2 ? 1 : 3));
      int offset_base = offset & ~mask;

      /* Check that all of the words can be accessed.  */
      if (4 < size && 0x80 < size + offset - offset_base)
	offset_base = offset & ~0x3f;

      if (offset_base && (offset & mask1) == 0)
	{
	  rtx temp = gen_rtx_PLUS (Pmode,
				   XEXP (newrtx, 0), GEN_INT (offset_base));

	  x = gen_rtx_PLUS (Pmode, temp, GEN_INT (offset - offset_base));
	  push_reload (XEXP (x, 0), 0, &XEXP (x, 0), 0,
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum,
		       (enum reload_type) type);
	  return x;
	}
    }

  return NULL_RTX;
}

/* Return the cost of moving data of mode MODE from a register in class FROM to
   one in class TO.  A value of 2 is the default; other values are interpreted
   relative to that.  */

static int
visium_register_move_cost (machine_mode mode, reg_class_t from,
			   reg_class_t to)
{
  const int numwords = (GET_MODE_SIZE (mode) <= UNITS_PER_WORD) ? 1 : 2;

  if (from == MDB || to == MDB)
    return 4;
  else if (from == MDC || to == MDC || (from == FP_REGS) != (to == FP_REGS))
    return 4 * numwords;
  else
    return 2 * numwords;
}

/* Return the cost of moving data of mode MODE between a register of class
   CLASS and memory.  IN is zero if the value is to be written to memory,
   non-zero if it is to be read in.  This cost is relative to those in
   visium_register_move_cost.  */

static int
visium_memory_move_cost (machine_mode mode,
			 reg_class_t to ATTRIBUTE_UNUSED,
			 bool in)
{
  /* Moving data in can be from PROM and this is expensive.  */
  if (in)
    {
      if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	return 7;
      else
	return 13;
    }

  /* Moving data out is mostly to RAM and should be cheaper.  */
  else
    {
      if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	return 6;
      else
	return 12;
    }
}

/* Return the relative costs of expression X.  */

static bool
visium_rtx_costs (rtx x, machine_mode mode, int outer_code ATTRIBUTE_UNUSED,
		  int opno ATTRIBUTE_UNUSED, int *total,
		  bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      /* Small integers are as cheap as registers.  4-byte values can
	 be fetched as immediate constants - let's give that the cost
	 of an extra insn.  */
      *total = COSTS_N_INSNS (!satisfies_constraint_J (x));
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (2);
      return true;

    case CONST_DOUBLE:
      {
	rtx high, low;
	split_double (x, &high, &low);
	*total =
	  COSTS_N_INSNS
	  (!satisfies_constraint_J (high) + !satisfies_constraint_J (low));
	return true;
      }

    case MULT:
      *total = COSTS_N_INSNS (3);
      return false;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      if (mode == DImode)
	*total = COSTS_N_INSNS (64);
      else
	*total = COSTS_N_INSNS (32);
      return false;

    case PLUS:
    case MINUS:
    case NEG:
      /* DImode operations are performed directly on the ALU.  */
      if (mode == DImode)
	*total = COSTS_N_INSNS (2);
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      /* DImode operations are performed on the EAM instead.  */
      if (mode == DImode)
	*total = COSTS_N_INSNS (3);
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case COMPARE:
      /* This matches the btst pattern.  */
      if (GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT
	  && XEXP (x, 1) == const0_rtx
	  && XEXP (XEXP (x, 0), 1) == const1_rtx
	  && satisfies_constraint_K (XEXP (XEXP (x, 0), 2)))
	*total = COSTS_N_INSNS (1);
      return false;

    default:
      return false;
    }
}

/* Split a double move of OPERANDS in MODE.  */

void
visium_split_double_move (rtx *operands, machine_mode mode)
{
  bool swap = false;

  /* Check register to register with overlap.  */
  if (GET_CODE (operands[0]) == REG
       && GET_CODE (operands[1]) == REG
       && REGNO (operands[0]) == REGNO (operands[1]) + 1)
    swap = true;

  /* Check memory to register where the base reg overlaps the destination.  */
  if (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == MEM)
    {
      rtx op = XEXP (operands[1], 0);

      if (GET_CODE (op) == SUBREG)
	op = SUBREG_REG (op);

      if (GET_CODE (op) == REG  && REGNO (op) == REGNO (operands[0]))
	swap = true;

      if (GET_CODE (op) == PLUS)
	{
	  rtx x = XEXP (op, 0);
	  rtx y = XEXP (op, 1);

	  if (GET_CODE (x) == REG && REGNO (x) == REGNO (operands[0]))
	    swap = true;

	  if (GET_CODE (y) == REG && REGNO (y) == REGNO (operands[0]))
	    swap = true;
	}
    }

  if (swap)
    {
      operands[2] = operand_subword (operands[0], 1, 1, mode);
      operands[3] = operand_subword (operands[1], 1, 1, mode);
      operands[4] = operand_subword (operands[0], 0, 1, mode);
      operands[5] = operand_subword (operands[1], 0, 1, mode);
    }
  else
    {
      operands[2] = operand_subword (operands[0], 0, 1, mode);
      operands[3] = operand_subword (operands[1], 0, 1, mode);
      operands[4] = operand_subword (operands[0], 1, 1, mode);
      operands[5] = operand_subword (operands[1], 1, 1, mode);
    }
}

/* Split a double addition or subtraction of operands.  */

void
visium_split_double_add (enum rtx_code code, rtx op0, rtx op1, rtx op2)
{
  rtx op3 = gen_lowpart (SImode, op0);
  rtx op4 = gen_lowpart (SImode, op1);
  rtx op5;
  rtx op6 = gen_highpart (SImode, op0);
  rtx op7 = (op1 == const0_rtx ? op1 : gen_highpart (SImode, op1));
  rtx op8;
  rtx x, pat, flags;

  /* If operand #2 is a small constant, then its high part is null.  */
  if (CONST_INT_P (op2))
    {
      HOST_WIDE_INT val = INTVAL (op2);

      if (val < 0)
	{
	  code = (code == MINUS ? PLUS : MINUS);
	  val = -val;
	}

      op5 = gen_int_mode (val, SImode);
      op8 = const0_rtx;
    }
  else
    {
      op5 = gen_lowpart (SImode, op2);
      op8 = gen_highpart (SImode, op2);
    }

  if (op4 == const0_rtx)
    pat = gen_negsi2_insn_set_carry (op3, op5);
  else if (code == MINUS)
    pat = gen_subsi3_insn_set_carry (op3, op4, op5);
  else
    pat = gen_addsi3_insn_set_carry (op3, op4, op5);
  emit_insn (pat);

  /* This is the plus_[plus_]sltu_flags or minus_[minus_]sltu_flags pattern.  */
  if (op8 == const0_rtx)
    x = op7;
  else
    x = gen_rtx_fmt_ee (code, SImode, op7, op8);
  flags = gen_rtx_REG (CCCmode, FLAGS_REGNUM);
  x = gen_rtx_fmt_ee (code, SImode, x, gen_rtx_LTU (SImode, flags, const0_rtx));
  pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
  XVECEXP (pat, 0, 0) = gen_rtx_SET (op6, x);
  flags = gen_rtx_REG (CCmode, FLAGS_REGNUM);
  XVECEXP (pat, 0, 1) = gen_rtx_CLOBBER (VOIDmode, flags);
  emit_insn (pat);

  visium_flags_exposed = true;
}

/* Expand a copysign of OPERANDS in MODE.  */

void
visium_expand_copysign (rtx *operands, machine_mode mode)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx mask = force_reg (SImode, GEN_INT (0x7fffffff));
  rtx x;

  /* We manually handle SFmode because the abs and neg instructions of
     the FPU on the MCM have a non-standard behavior wrt NaNs.  */
  gcc_assert (mode == SFmode);

  /* First get all the non-sign bits of op1.  */
  if (GET_CODE (op1) == CONST_DOUBLE)
    {
      if (real_isneg (CONST_DOUBLE_REAL_VALUE (op1)))
	op1 = simplify_unary_operation (ABS, mode, op1, mode);
      if (op1 != CONST0_RTX (mode))
	{
	  long l;
	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op1), l);
	  op1 = force_reg (SImode, gen_int_mode (l, SImode));
	}
    }
  else
    {
      op1 = copy_to_mode_reg (SImode, gen_lowpart (SImode, op1));
      op1 = force_reg (SImode, gen_rtx_AND (SImode, op1, mask));
    }

  /* Then get the sign bit of op2.  */
  mask = force_reg (SImode, gen_rtx_NOT (SImode, mask));
  op2 = copy_to_mode_reg (SImode, gen_lowpart (SImode, op2));
  op2 = force_reg (SImode, gen_rtx_AND (SImode, op2, mask));

  /* Finally OR the two values.  */
  if (op1 == CONST0_RTX (SFmode))
    x = op2;
  else
    x = force_reg (SImode, gen_rtx_IOR (SImode, op1, op2));

  /* And move the result to the destination.  */
  emit_insn (gen_rtx_SET (op0, gen_lowpart (SFmode, x)));
}

/* Expand a cstore of OPERANDS in MODE for EQ/NE/LTU/GTU/GEU/LEU.  We generate
   the result in the C flag and use the ADC/SUBC instructions to write it into
   the destination register.

   It would also be possible to implement support for LT/GT/LE/GE by means of
   the RFLAG instruction followed by some shifts, but this can pessimize the
   generated code.  */

void
visium_expand_int_cstore (rtx *operands, machine_mode mode)
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[0], op1 = operands[2], op2 = operands[3], sltu;
  bool reverse = false;

  switch (code)
    {
    case EQ:
    case NE:
      /* We use a special comparison to get the result in the C flag.  */
      if (op2 != const0_rtx)
	op1 = force_reg (mode, gen_rtx_XOR (mode, op1, op2));
      op1 = gen_rtx_NOT (mode, op1);
      op2 = constm1_rtx;
      if (code == EQ)
	reverse = true;
      break;

    case LEU:
    case GEU:
      /* The result is naturally in the C flag modulo a couple of tricks.  */
      code = reverse_condition (code);
      reverse = true;

      /* ... fall through ...  */

    case LTU:
    case GTU:
      if (code == GTU)
	{
	  rtx tmp = op1;
	  op1 = op2;
	  op2 = tmp;
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* We need either a single ADC or a SUBC and a PLUS.  */
  sltu = gen_rtx_LTU (SImode, op1, op2);

  if (reverse)
    {
      rtx tmp = copy_to_mode_reg (SImode, gen_rtx_NEG (SImode, sltu));
      emit_insn (gen_add3_insn (op0, tmp, const1_rtx));
    }
  else
    emit_insn (gen_rtx_SET (op0, sltu));
}

/* Expand a cstore of OPERANDS in MODE for LT/GT/UNGE/UNLE.  We generate the
   result in the C flag and use the ADC/SUBC instructions to write it into
   the destination register.  */

void
visium_expand_fp_cstore (rtx *operands,
			 machine_mode mode ATTRIBUTE_UNUSED)
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[0], op1 = operands[2], op2 = operands[3], slt;
  bool reverse = false;

  switch (code)
    {
    case UNLE:
    case UNGE:
      /* The result is naturally in the C flag modulo a couple of tricks.  */
      code = reverse_condition_maybe_unordered (code);
      reverse = true;

      /* ... fall through ...  */

    case LT:
    case GT:
      if (code == GT)
	{
	  rtx tmp = op1;
	  op1 = op2;
	  op2 = tmp;
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* We need either a single ADC or a SUBC and a PLUS.  */
  slt = gen_rtx_LT (SImode, op1, op2);

  if (reverse)
    {
      rtx tmp = copy_to_mode_reg (SImode, gen_rtx_NEG (SImode, slt));
      emit_insn (gen_add3_insn (op0, tmp, const1_rtx));
    }
  else
    emit_insn (gen_rtx_SET (op0, slt));
}

/* Split a compare-and-store with CODE, operands OP2 and OP3, combined with
   operation with OP_CODE, operands OP0 and OP1.  */

void
visium_split_cstore (enum rtx_code op_code, rtx op0, rtx op1,
		     enum rtx_code code, rtx op2, rtx op3)
{
  machine_mode cc_mode = visium_select_cc_mode (code, op2, op3);

  /* If a FP cstore was reversed, then it was originally UNGE/UNLE.  */
  if (cc_mode == CCFPEmode && (op_code == NEG || op_code == MINUS))
    cc_mode = CCFPmode;

  rtx flags = gen_rtx_REG (cc_mode, FLAGS_REGNUM);
  rtx x = gen_rtx_COMPARE (cc_mode, op2, op3);
  x = gen_rtx_SET (flags, x);
  emit_insn (x);

  x = gen_rtx_fmt_ee (code, SImode, flags, const0_rtx);
  switch (op_code)
    {
    case SET:
      break;
    case NEG:
      x = gen_rtx_NEG (SImode, x);
      break;
    case PLUS:
    case MINUS:
      x = gen_rtx_fmt_ee (op_code, SImode, op1, x);
      break;
    default:
      gcc_unreachable ();
    }

  rtx pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
  XVECEXP (pat, 0, 0) = gen_rtx_SET (op0, x);
  flags = gen_rtx_REG (CCmode, FLAGS_REGNUM);
  XVECEXP (pat, 0, 1) = gen_rtx_CLOBBER (VOIDmode, flags);
  emit_insn (pat);

  visium_flags_exposed = true;
}

/* Generate a call to a library function to move BYTES_RTX bytes from SRC with
   address SRC_REG to DST with address DST_REG in 4-byte chunks.  */

static void
expand_block_move_4 (rtx dst, rtx dst_reg, rtx src, rtx src_reg, rtx bytes_rtx)
{
  unsigned HOST_WIDE_INT bytes = UINTVAL (bytes_rtx);
  unsigned int rem = bytes % 4;

  if (TARGET_BMI)
    {
      unsigned int i;
      rtx insn;

      emit_move_insn (regno_reg_rtx[1], dst_reg);
      emit_move_insn (regno_reg_rtx[2], src_reg);
      emit_move_insn (regno_reg_rtx[3], bytes_rtx);

      insn = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (8));
      XVECEXP (insn, 0, 0)
	= gen_rtx_SET (replace_equiv_address_nv (dst, regno_reg_rtx[1]),
		       replace_equiv_address_nv (src, regno_reg_rtx[2]));
      XVECEXP (insn, 0, 1) = gen_rtx_USE (VOIDmode, regno_reg_rtx[3]);
      for (i = 1; i <= 6; i++)
	XVECEXP (insn, 0, 1 + i)
	  = gen_rtx_CLOBBER (VOIDmode, regno_reg_rtx[i]);
      emit_insn (insn);
    }
  else
    emit_library_call (long_int_memcpy_libfunc, LCT_NORMAL, VOIDmode,
		       dst_reg, Pmode,
		       src_reg, Pmode,
		       convert_to_mode (TYPE_MODE (sizetype),
					GEN_INT (bytes >> 2),
				        TYPE_UNSIGNED (sizetype)),
		       TYPE_MODE (sizetype));
  if (rem == 0)
    return;

  dst = replace_equiv_address_nv (dst, dst_reg);
  src = replace_equiv_address_nv (src, src_reg);
  bytes -= rem;

  if (rem > 1)
    {
      emit_move_insn (adjust_address_nv (dst, HImode, bytes),
		      adjust_address_nv (src, HImode, bytes));
      bytes += 2;
      rem -= 2;
    }

  if (rem > 0)
    emit_move_insn (adjust_address_nv (dst, QImode, bytes),
		    adjust_address_nv (src, QImode, bytes));
}

/* Generate a call to a library function to move BYTES_RTX bytes from SRC with
   address SRC_REG to DST with address DST_REG in 2-bytes chunks.  */

static void
expand_block_move_2 (rtx dst, rtx dst_reg, rtx src, rtx src_reg, rtx bytes_rtx)
{
  unsigned HOST_WIDE_INT bytes = UINTVAL (bytes_rtx);
  unsigned int rem = bytes % 2;

  emit_library_call (wrd_memcpy_libfunc, LCT_NORMAL, VOIDmode,
		     dst_reg, Pmode,
		     src_reg, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype),
				      GEN_INT (bytes >> 1),
				      TYPE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
  if (rem == 0)
    return;

  dst = replace_equiv_address_nv (dst, dst_reg);
  src = replace_equiv_address_nv (src, src_reg);
  bytes -= rem;

  emit_move_insn (adjust_address_nv (dst, QImode, bytes),
		  adjust_address_nv (src, QImode, bytes));
}

/* Generate a call to a library function to move BYTES_RTX bytes from address
   SRC_REG to address DST_REG in 1-byte chunks.  */

static void
expand_block_move_1 (rtx dst_reg, rtx src_reg, rtx bytes_rtx)
{
  emit_library_call (byt_memcpy_libfunc, LCT_NORMAL, VOIDmode,
		     dst_reg, Pmode,
		     src_reg, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype),
				      bytes_rtx,
				      TYPE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
}

/* Generate a call to a library function to set BYTES_RTX bytes of DST with
   address DST_REG to VALUE_RTX in 4-byte chunks.  */

static void
expand_block_set_4 (rtx dst, rtx dst_reg, rtx value_rtx, rtx bytes_rtx)
{
  unsigned HOST_WIDE_INT bytes = UINTVAL (bytes_rtx);
  unsigned int rem = bytes % 4;

  value_rtx = convert_to_mode (Pmode, value_rtx, 1);
  emit_library_call (long_int_memset_libfunc, LCT_NORMAL, VOIDmode,
		     dst_reg, Pmode,
		     value_rtx, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype),
				      GEN_INT (bytes >> 2),
				      TYPE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
  if (rem == 0)
    return;

  dst = replace_equiv_address_nv (dst, dst_reg);
  bytes -= rem;

  if (rem > 1)
    {
      if (CONST_INT_P (value_rtx))
	{
	  const unsigned HOST_WIDE_INT value = UINTVAL (value_rtx) & 0xff;
	  emit_move_insn (adjust_address_nv (dst, HImode, bytes),
			  gen_int_mode ((value << 8) | value, HImode));
	}
      else
	{
	  rtx temp = convert_to_mode (QImode, value_rtx, 1);
	  emit_move_insn (adjust_address_nv (dst, QImode, bytes), temp);
	  emit_move_insn (adjust_address_nv (dst, QImode, bytes + 1), temp);
	}
      bytes += 2;
      rem -= 2;
    }

  if (rem > 0)
    emit_move_insn (adjust_address_nv (dst, QImode, bytes),
		    convert_to_mode (QImode, value_rtx, 1));
}

/* Generate a call to a library function to set BYTES_RTX bytes of DST with
   address DST_REG to VALUE_RTX in 2-byte chunks.  */

static void
expand_block_set_2 (rtx dst, rtx dst_reg, rtx value_rtx, rtx bytes_rtx)
{
  unsigned HOST_WIDE_INT bytes = UINTVAL (bytes_rtx);
  unsigned int rem = bytes % 2;

  value_rtx = convert_to_mode (Pmode, value_rtx, 1);
  emit_library_call (wrd_memset_libfunc, LCT_NORMAL, VOIDmode,
		     dst_reg, Pmode,
		     value_rtx, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype),
				      GEN_INT (bytes >> 1),
				      TYPE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
  if (rem == 0)
    return;

  dst = replace_equiv_address_nv (dst, dst_reg);
  bytes -= rem;

  emit_move_insn (adjust_address_nv (dst, QImode, bytes),
		  convert_to_mode (QImode, value_rtx, 1));
}

/* Generate a call to a library function to set BYTES_RTX bytes at address
   DST_REG to VALUE_RTX in 1-byte chunks.  */

static void
expand_block_set_1 (rtx dst_reg, rtx value_rtx, rtx bytes_rtx)
{
  value_rtx = convert_to_mode (Pmode, value_rtx, 1);
  emit_library_call (byt_memset_libfunc, LCT_NORMAL, VOIDmode,
		     dst_reg, Pmode,
		     value_rtx, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype),
				      bytes_rtx,
				      TYPE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
}

/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.

   Return 1 upon success, 0 otherwise.  */

int
visium_expand_block_move (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx bytes_rtx = operands[2];
  rtx align_rtx = operands[3];
  const int align = INTVAL (align_rtx);
  rtx dst_reg, src_reg;
  tree dst_expr, src_expr;

  /* We only handle a fixed number of bytes for now.  */
  if (!CONST_INT_P (bytes_rtx) || INTVAL (bytes_rtx) <= 0)
    return 0;

  /* Copy the addresses into scratch registers.  */
  dst_reg = copy_addr_to_reg (XEXP (dst, 0));
  src_reg = copy_addr_to_reg (XEXP (src, 0));

  /* Move the data with the appropriate granularity.  */
  if (align >= 4)
    expand_block_move_4 (dst, dst_reg, src, src_reg, bytes_rtx);
  else if (align >= 2)
    expand_block_move_2 (dst, dst_reg, src, src_reg, bytes_rtx);
  else
    expand_block_move_1 (dst_reg, src_reg, bytes_rtx);

  /* Since DST and SRC are passed to a libcall, mark the corresponding
     tree EXPR as addressable.  */
  dst_expr = MEM_EXPR (dst);
  src_expr = MEM_EXPR (src);
  if (dst_expr)
    mark_addressable (dst_expr);
  if (src_expr)
    mark_addressable (src_expr);

  return 1;
}

/* Expand string/block set operations.

   operands[0] is the pointer to the destination.
   operands[1] is the number of bytes to set.
   operands[2] is the source value.
   operands[3] is the alignment.

   Return 1 upon success, 0 otherwise.  */

int
visium_expand_block_set (rtx *operands)
{
  rtx dst = operands[0];
  rtx bytes_rtx = operands[1];
  rtx value_rtx = operands[2];
  rtx align_rtx = operands[3];
  const int align = INTVAL (align_rtx);
  rtx dst_reg;
  tree dst_expr;

  /* We only handle a fixed number of bytes for now.  */
  if (!CONST_INT_P (bytes_rtx) || INTVAL (bytes_rtx) <= 0)
    return 0;

  /* Copy the address into a scratch register.  */
  dst_reg = copy_addr_to_reg (XEXP (dst, 0));

  /* Set the data with the appropriate granularity.  */
  if (align >= 4)
    expand_block_set_4 (dst, dst_reg, value_rtx, bytes_rtx);
  else if (align >= 2)
    expand_block_set_2 (dst, dst_reg, value_rtx, bytes_rtx);
  else
    expand_block_set_1 (dst_reg, value_rtx, bytes_rtx);

  /* Since DST is passed to a libcall, mark the corresponding
     tree EXPR as addressable.  */
  dst_expr = MEM_EXPR (dst);
  if (dst_expr)
    mark_addressable (dst_expr);

  return 1;
}

/* Initialize a trampoline.  M_TRAMP is an RTX for the memory block for the
   trampoline, FNDECL is the FUNCTION_DECL for the nested function and
   STATIC_CHAIN is an RTX for the static chain value that should be passed
   to the function when it is called.  */

static void
visium_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx addr = XEXP (m_tramp, 0);

  /* The trampoline initialization sequence is:

	moviu   r9,%u FUNCTION
	movil   r9,%l FUNCTION
	moviu   r20,%u STATIC
	bra     tr,r9,r9
	 movil   r20,%l STATIC

     We don't use r0 as the destination register of the branch because we want
     the Branch Pre-decode Logic of the GR6 to use the Address Load Array to
     predict the branch target.  */

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, addr, 0)),
		  plus_constant (SImode,
				 expand_shift (RSHIFT_EXPR, SImode, fnaddr,
					       16, NULL_RTX, 1),
				 0x04a90000));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, addr, 4)),
		  plus_constant (SImode,
				 expand_and (SImode, fnaddr, GEN_INT (0xffff),
					     NULL_RTX),
				 0x04890000));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, addr, 8)),
		  plus_constant (SImode,
				 expand_shift (RSHIFT_EXPR, SImode,
					       static_chain,
					       16, NULL_RTX, 1),
				 0x04b40000));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, addr, 12)),
		  gen_int_mode (0xff892404, SImode));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, addr, 16)),
		  plus_constant (SImode,
				 expand_and (SImode, static_chain,
					     GEN_INT (0xffff), NULL_RTX),
				 0x04940000));

  emit_library_call (set_trampoline_parity_libfunc, LCT_NORMAL, VOIDmode,
		     addr, SImode);
}

/* Return true if the current function must have and use a frame pointer.  */

static bool
visium_frame_pointer_required (void)
{
  /* The frame pointer is required if the function isn't leaf to be able to
     do manual stack unwinding.  */
  if (!crtl->is_leaf)
    return true;

  /* If the stack pointer is dynamically modified in the function, it cannot
     serve as the frame pointer.  */
  if (!crtl->sp_is_unchanging)
    return true;

  /* If the function receives nonlocal gotos, it needs to save the frame
     pointer in the nonlocal_goto_save_area object.  */
  if (cfun->has_nonlocal_label)
    return true;

  /* The frame also needs to be established in some special cases.  */
  if (visium_frame_needed)
    return true;

  return false;
}

/* Profiling support.  Just a call to MCOUNT is needed.  No labelled counter
   location is involved.  Proper support for __builtin_return_address is also
   required, which is fairly straightforward provided a frame gets created.  */

void
visium_profile_hook (void)
{
  visium_frame_needed = true;
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "mcount"), LCT_NORMAL,
		     VOIDmode);
}

/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */

rtx
visium_dynamic_chain_address (rtx frame)
{
  /* This is the default, but we need to make sure the frame gets created.  */
  visium_frame_needed = true;
  return frame;
}

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */

rtx
visium_return_addr_rtx (int count, rtx frame ATTRIBUTE_UNUSED)
{
  /* Dont try to compute anything other than frame zero.  */
  if (count != 0)
    return NULL_RTX;

  visium_frame_needed = true;
  return
    gen_frame_mem (Pmode, plus_constant (Pmode, hard_frame_pointer_rtx, 4));
}

/* Helper function for EH_RETURN_HANDLER_RTX.  Return the RTX representing a
   location in which to store the address of an exception handler to which we
   should return.  */

rtx
visium_eh_return_handler_rtx (void)
{
  rtx mem
    = gen_frame_mem (SImode, plus_constant (Pmode, hard_frame_pointer_rtx, 4));
  MEM_VOLATILE_P (mem) = 1;
  return mem;
}

static struct machine_function *
visium_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* The per-function data machinery is needed to indicate when a frame
   is required. */

void
visium_init_expanders (void)
{
  init_machine_status = visium_init_machine_status;
}

/* Given a comparison code (EQ, NE, etc.) and the operands of a COMPARE,
   return the mode to be used for the comparison.  */

machine_mode
visium_select_cc_mode (enum rtx_code code, rtx op0, rtx op1)
{
  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    {
      switch (code)
	{
	case EQ:
	case NE:
	case ORDERED:
	case UNORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	/* These 2 comparison codes are not supported.  */
	case UNEQ:
	case LTGT:
	default:
	  gcc_unreachable ();
	}
    }

  /* This is for the cmp<mode>_sne pattern.  */
  if (op1 == constm1_rtx)
    return CCCmode;

  /* This is for the add<mode>3_insn_set_carry pattern.  */
  if ((code == LTU || code == GEU)
      && GET_CODE (op0) == PLUS
      && rtx_equal_p (XEXP (op0, 0), op1))
    return CCCmode;

  /* This is for the {add,sub,neg}<mode>3_insn_set_overflow pattern.  */
  if ((code == EQ || code == NE)
      && GET_CODE (op1) == UNSPEC
      && (XINT (op1, 1) == UNSPEC_ADDV
	  || XINT (op1, 1) == UNSPEC_SUBV
	  || XINT (op1, 1) == UNSPEC_NEGV))
    return CCVmode;

  if (op1 != const0_rtx)
    return CCmode;

  switch (GET_CODE (op0))
    {
    case PLUS:
    case MINUS:
    case NEG:
    case ASHIFT:
    case LTU:
    case LT:
      /* The C and V flags may be set differently from a COMPARE with zero.
	 The consequence is that a comparison operator testing C or V must
	 be turned into another operator not testing C or V and yielding
	 the same result for a comparison with zero.  That's possible for
	 GE/LT which become NC/NS respectively, but not for GT/LE for which
	 the altered operator doesn't exist on the Visium.  */
      return CCNZmode;

    case ZERO_EXTRACT:
      /* This is a btst, the result is in C instead of Z.  */
      return CCCmode;

    case CONST_INT:
      /* This is a degenerate case, typically an uninitialized variable.  */
      gcc_assert (op0 == constm1_rtx);

      /* ... fall through ... */

    case REG:
    case AND:
    case IOR:
    case XOR:
    case NOT:
    case ASHIFTRT:
    case LSHIFTRT:
    case TRUNCATE:
    case SIGN_EXTEND:
      /* Pretend that the flags are set as for a COMPARE with zero.
	 That's mostly true, except for the 2 right shift insns that
	 will set the C flag.  But the C flag is relevant only for
	 the unsigned comparison operators and they are eliminated
	 when applied to a comparison with zero.  */
      return CCmode;

    default:
      gcc_unreachable ();
    }
}

/* Split a compare-and-branch with CODE, operands OP0 and OP1, and LABEL.  */

void
visium_split_cbranch (enum rtx_code code, rtx op0, rtx op1, rtx label)
{
  machine_mode cc_mode = visium_select_cc_mode (code, op0, op1);
  rtx flags = gen_rtx_REG (cc_mode, FLAGS_REGNUM);

  rtx x = gen_rtx_COMPARE (cc_mode, op0, op1);
  x = gen_rtx_SET (flags, x);
  emit_insn (x);

  x = gen_rtx_fmt_ee (code, VOIDmode, flags, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, gen_rtx_LABEL_REF (Pmode, label),
			    pc_rtx);
  x = gen_rtx_SET (pc_rtx, x);
  emit_jump_insn (x);

  visium_flags_exposed = true;
}

/* Branch instructions on the Visium.

   Setting aside the interrupt-handling specific instructions, the ISA has
   two branch instructions: BRR and BRA.  The former is used to implement
   short branches (+/- 2^17) within functions and its target is encoded in
   the instruction.  The latter is used to implement all the other types
   of control flow changes and its target might not be statically known
   or even easily predictable at run time.  Here's a complete summary of
   the patterns that generate a BRA instruction:

     1. Indirect jump
     2. Table jump
     3. Call
     4. Sibling call
     5. Return
     6. Long branch
     7. Trampoline

   Among these patterns, only the return (5) and the long branch (6) can be
   conditional; all the other patterns are always unconditional.

   The following algorithm can be used to identify the pattern for which
   the BRA instruction was generated and work out its target:

     A. If the source is r21 and the destination is r0, this is a return (5)
	and the target is the caller (i.e. the value of r21 on function's
	entry).

     B. If the source is rN, N != 21 and the destination is r0, this is either
	an indirect jump or a table jump (1, 2) and the target is not easily
	predictable.

     C. If the source is rN, N != 21 and the destination is r21, this is a call
	(3) and the target is given by the preceding MOVIL/MOVIU pair for rN,
	unless this is an indirect call in which case the target is not easily
	predictable.

     D. If the source is rN, N != 21 and the destination is also rN, this is
	either a sibling call or a trampoline (4, 7) and the target is given
	by the preceding MOVIL/MOVIU pair for rN.

     E. If the source is r21 and the destination is also r21, this is a long
	branch (6) and the target is given by the preceding MOVIL/MOVIU pair
	for r21.

   The other combinations are not used.  This implementation has been devised
   to accommodate the branch predictor of the GR6 but is used unconditionally
   by the compiler, i.e. including for earlier processors.  */

/* Output a conditional/unconditional branch to LABEL.  COND is the string
   condition.  INSN is the instruction.  */

static const char *
output_branch (rtx label, const char *cond, rtx_insn *insn)
{
  char str[64];
  rtx operands[2];

  gcc_assert (cond);
  operands[0] = label;

  /* If the length of the instruction is greater than 8, then this is a
     long branch and we need to work harder to emit it properly.  */
  if (get_attr_length (insn) > 8)
    {
      bool spilled;

      /* If the link register has been saved, then we use it.  */
      if (current_function_saves_lr ())
	{
	  operands[1] = regno_reg_rtx [LINK_REGNUM];
	  spilled = false;
	}

      /* Or else, if the long-branch register isn't live, we use it.  */
      else if (!df_regs_ever_live_p (long_branch_regnum))
	{
	  operands[1] = regno_reg_rtx [long_branch_regnum];
	  spilled = false;
	}

      /* Otherwise, we will use the long-branch register but we need to
	 spill it to the stack and reload it at the end.  We should have
	 reserved the LR slot for this purpose.  */
      else
	{
	  operands[1] = regno_reg_rtx [long_branch_regnum];
	  spilled = true;
	  gcc_assert (current_function_has_lr_slot ());
	}

      /* First emit the spill to the stack:

	   insn_in_delay_slot
	   write.l [1](sp),reg  */
      if (spilled)
	{
	  if (final_sequence)
	    {
	      rtx_insn *delay = NEXT_INSN (insn);
	      int seen;
	      gcc_assert (delay);

	      final_scan_insn (delay, asm_out_file, optimize, 0, &seen);
	      PATTERN (delay) = gen_blockage ();
	      INSN_CODE (delay) = -1;
	    }

	  if (current_function_saves_fp ())
	    output_asm_insn ("write.l 1(sp),%1", operands);
	  else
	    output_asm_insn ("write.l (sp),%1", operands);
	}

      /* Then emit the core sequence:

	   moviu   reg,%u label
	   movil   reg,%l label
	   bra     tr,reg,reg

	 We don't use r0 as the destination register of the branch because we
	 want the Branch Pre-decode Logic of the GR6 to use the Address Load
	 Array to predict the branch target.  */
      output_asm_insn ("moviu   %1,%%u %0", operands);
      output_asm_insn ("movil   %1,%%l %0", operands);
      strcpy (str, "bra     ");
      strcat (str, cond);
      strcat (str, ",%1,%1");
      if (!spilled)
	strcat (str, "%#");
      strcat (str, "\t\t;long branch");
      output_asm_insn (str, operands);

      /* Finally emit the reload:

	    read.l reg,[1](sp)  */
      if (spilled)
	{
	  if (current_function_saves_fp ())
	    output_asm_insn (" read.l %1,1(sp)", operands);
	  else
	    output_asm_insn (" read.l %1,(sp)", operands);
	}
    }

  /* Or else, if the label is PC, then this is a return.  */
  else if (label == pc_rtx)
    {
      strcpy (str, "bra     ");
      strcat (str, cond);
      strcat (str, ",r21,r0%#\t\t;return");
      output_asm_insn (str, operands);
    }

  /* Otherwise, this is a short branch.  */
  else
    {
      strcpy (str, "brr     ");
      strcat (str, cond);
      strcat (str, ",%0%#");
      output_asm_insn (str, operands);
    }

  return "";
}

/* Output an unconditional branch to LABEL.  INSN is the instruction.  */

const char *
output_ubranch (rtx label, rtx_insn *insn)
{
  return output_branch (label, "tr", insn);
}

/* Output a conditional branch to LABEL.  CODE is the comparison code.
   CC_MODE is the mode of the CC register.  REVERSED is non-zero if we
   should reverse the sense of the comparison.  INSN is the instruction.  */

const char *
output_cbranch (rtx label, enum rtx_code code, machine_mode cc_mode,
		int reversed, rtx_insn *insn)
{
  const char *cond;

  if (reversed)
    {
      if (cc_mode == CCFPmode || cc_mode == CCFPEmode)
	code = reverse_condition_maybe_unordered (code);
      else
	code = reverse_condition (code);
    }

  switch (code)
    {
    case NE:
      if (cc_mode == CCCmode)
	cond = "cs";
      else if (cc_mode == CCVmode)
	cond = "os";
      else
	cond = "ne";
      break;

    case EQ:
      if (cc_mode == CCCmode)
	cond = "cc";
      else if (cc_mode == CCVmode)
	cond = "oc";
      else
	cond = "eq";
      break;

    case GE:
      if (cc_mode == CCNZmode)
	cond = "nc";
      else
	cond = "ge";
      break;

    case GT:
      cond = "gt";
      break;

    case LE:
      if (cc_mode == CCFPmode || cc_mode == CCFPEmode)
	cond = "ls";
      else
	cond = "le";
      break;

    case LT:
      if (cc_mode == CCFPmode || cc_mode == CCFPEmode)
	cond = "cs"; /* or "ns" */
      else if (cc_mode == CCNZmode)
	cond = "ns";
      else
	cond = "lt";
      break;

    case GEU:
      cond = "cc";
      break;

    case GTU:
      cond = "hi";
      break;

    case LEU:
      cond = "ls";
      break;

    case LTU:
      cond = "cs";
      break;

    case UNORDERED:
      cond = "os";
      break;

    case ORDERED:
      cond = "oc";
      break;

    case UNGE:
      cond = "cc"; /* or "nc" */
      break;

    case UNGT:
      cond = "hi";
      break;

    case UNLE:
      cond = "le";
      break;

    case UNLT:
      cond = "lt";
      break;

    /* These 2 comparison codes are not supported.  */
    case UNEQ:
    case LTGT:
    default:
      gcc_unreachable ();
    }

  return output_branch (label, cond, insn);
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
visium_print_operand_punct_valid_p (unsigned char code)
{
  return code == '#';
}

/* Implement TARGET_PRINT_OPERAND.  Output to stdio stream FILE the assembler
   syntax for an instruction operand OP subject to the modifier LETTER.  */

static void
visium_print_operand (FILE *file, rtx op, int letter)
{
  switch (letter)
    {
    case '#':
      /* Output an insn in a delay slot.  */
      if (final_sequence)
	visium_indent_opcode = 1;
      else
	fputs ("\n\t nop", file);
      return;

    case 'b':
      /* Print LS 8 bits of operand.  */
      fprintf (file, HOST_WIDE_INT_PRINT_UNSIGNED, UINTVAL (op) & 0xff);
      return;

    case 'w':
      /* Print LS 16 bits of operand.  */
      fprintf (file, HOST_WIDE_INT_PRINT_UNSIGNED, UINTVAL (op) & 0xffff);
      return;

    case 'u':
      /* Print MS 16 bits of operand.  */
      fprintf (file,
	       HOST_WIDE_INT_PRINT_UNSIGNED, (UINTVAL (op) >> 16) & 0xffff);
      return;

    case 'r':
      /* It's either a register or zero.  */
      if (GET_CODE (op) == REG)
	fputs (reg_names[REGNO (op)], file);
      else
	fputs (reg_names[0], file);
      return;

    case 'f':
      /* It's either a FP register or zero.  */
       if (GET_CODE (op) == REG)
	fputs (reg_names[REGNO (op)], file);
       else
	fputs (reg_names[FP_FIRST_REGNUM], file);
       return;
    }

  switch (GET_CODE (op))
    {
    case REG:
      if (letter == 'd')
	fputs (reg_names[REGNO (op) + 1], file);
      else
	fputs (reg_names[REGNO (op)], file);
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      output_addr_const (file, op);
      break;

    case MEM:
      visium_print_operand_address (file, GET_MODE (op), XEXP (op, 0));
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (op));
      break;

    case CODE_LABEL:
      asm_fprintf (file, "%LL%d", CODE_LABEL_NUMBER (op));
      break;

    case HIGH:
      visium_print_operand (file, XEXP (op, 1), letter);
      break;

    default:
      fatal_insn ("illegal operand ", op);
    }
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  Output to stdio stream FILE the
   assembler syntax for an instruction operand that is a memory reference
   whose address is ADDR.  */

static void
visium_print_operand_address (FILE *file, machine_mode mode, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case REG:
    case SUBREG:
      fprintf (file, "(%s)", reg_names[true_regnum (addr)]);
      break;

    case PLUS:
      {
	rtx x = XEXP (addr, 0), y = XEXP (addr, 1);

	switch (GET_CODE (x))
	  {
	  case REG:
	  case SUBREG:
	    if (CONST_INT_P (y))
	      {
		unsigned int regno = true_regnum (x);
		HOST_WIDE_INT val = INTVAL (y);
		switch (mode)
		  {
		  case E_SImode:
		  case E_DImode:
		  case E_SFmode:
		  case E_DFmode:
		    val >>= 2;
		    break;

		  case E_HImode:
		    val >>= 1;
		    break;

		  case E_QImode:
		  default:
		    break;
		  }
	        fprintf (file, HOST_WIDE_INT_PRINT_DEC"(%s)", val,
			 reg_names[regno]);
	      }
	    else
	      fatal_insn ("illegal operand address (1)", addr);
	    break;

	  default:
	    if (CONSTANT_P (x) && CONSTANT_P (y))
	      output_addr_const (file, addr);
	    else
	      fatal_insn ("illegal operand address (2)", addr);
	    break;
	  }
      }
      break;

    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
      output_addr_const (file, addr);
      break;

    case NOTE:
      if (NOTE_KIND (addr) != NOTE_INSN_DELETED_LABEL)
	fatal_insn ("illegal operand address (3)", addr);
      break;

    case CODE_LABEL:
      asm_fprintf (file, "%LL%d", CODE_LABEL_NUMBER (addr));
      break;

    default:
      fatal_insn ("illegal operand address (4)", addr);
      break;
    }
}

/* The Visium stack frames look like:

              Before call                      After call
        +-----------------------+       +-----------------------+
        |                       |       |                       |
   high |      previous         |       |      previous         |
   mem  |      frame            |       |      frame            |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  arguments on stack   |       |  arguments on stack   |
        |                       |       |                       |
  SP+0->+-----------------------+       +-----------------------+
                                        |  reg parm save area,  |
                                        |  only created for     |
                                        |  variable argument    |
                                        |  functions            |
                                        +-----------------------+
                                        |                       |
                                        |  register save area   |
                                        |                       |
                                        +-----------------------+
                                        |                       |
                                        |  local variables      |
                                        |                       |
                                  FP+8->+-----------------------+
                                        |    return address     |
                                  FP+4->+-----------------------+
                                        |      previous FP      |
                                  FP+0->+-----------------------+
                                        |                       |
                                        |  alloca allocations   |
                                        |                       |
                                        +-----------------------+
                                        |                       |
   low                                  |  arguments on stack   |
   mem                                  |                       |
                                  SP+0->+-----------------------+

   Notes:
   1) The "reg parm save area" does not exist for non variable argument fns.
   2) The FP register is not saved if `frame_pointer_needed' is zero and it
      is not altered in the current function.
   3) The return address is not saved if there is no frame pointer and the
      current function is leaf.
   4) If the return address is not saved and the static chain register is
      live in the function, we allocate the return address slot to be able
      to spill the register for a long branch.  */

/* Define the register classes for local purposes.  */
enum reg_type { general, mdb, mdc, floating, last_type};

#define GET_REG_TYPE(regno)          \
  (GP_REGISTER_P (regno) ? general : \
   (regno) == MDB_REGNUM ? mdb :      \
   (regno) == MDC_REGNUM ? mdc :      \
   floating)

/* First regno of each register type.  */
const int first_regno[last_type] = {0, MDB_REGNUM, MDC_REGNUM, FP_FIRST_REGNUM};

/* Size in bytes of each register type.  */
const int reg_type_size[last_type] = {4, 8, 4, 4};

/* Structure to be filled in by visium_compute_frame_size.  */
struct visium_frame_info
{
  unsigned int save_area_size;	/* # bytes in the reg parm save area.  */
  unsigned int reg_size1;	/* # bytes to store first block of regs.  */
  unsigned int reg_size2;	/* # bytes to store second block of regs.  */
  unsigned int max_reg1;	/* max. regno in first block */
  unsigned int var_size;	/* # bytes that variables take up.  */
  unsigned int save_fp;		/* Nonzero if fp must be saved.  */
  unsigned int save_lr;		/* Nonzero if lr must be saved.  */
  unsigned int lr_slot;		/* Nonzero if the lr slot is needed.  */
  unsigned int combine;		/* Nonzero if we can combine the allocation of
				   variables and regs. */
  unsigned int interrupt;	/* Nonzero if the function is an interrupt
				   handler. */
  unsigned int mask[last_type];	/* Masks of saved regs: gp, mdb, mdc, fp */
};

/* Current frame information calculated by visium_compute_frame_size.  */
static struct visium_frame_info current_frame_info;

/* Accessor for current_frame_info.save_fp.  */

static inline bool
current_function_saves_fp (void)
{
  return current_frame_info.save_fp != 0;
}

/* Accessor for current_frame_info.save_lr.  */

static inline bool
current_function_saves_lr (void)
{
  return current_frame_info.save_lr != 0;
}

/* Accessor for current_frame_info.lr_slot.  */

static inline bool
current_function_has_lr_slot (void)
{
  return current_frame_info.lr_slot != 0;
}

/* Return non-zero if register REGNO needs to be saved in the frame.  */

static int
visium_save_reg_p (int interrupt, int regno)
{
  switch (regno)
    {
    case HARD_FRAME_POINTER_REGNUM:
      /* This register is call-saved but handled specially.  */
      return 0;

    case MDC_REGNUM:
      /* This register is fixed but can be modified.  */
      break;

    case 29:
    case 30:
      /* These registers are fixed and hold the interrupt context.  */
      return (interrupt != 0);

    default:
      /* The other fixed registers are either immutable or special.  */
      if (fixed_regs[regno])
	return 0;
      break;
    }

  if (interrupt)
    {
      if (crtl->is_leaf)
	{
	  if (df_regs_ever_live_p (regno))
	    return 1;
	}
      else if (call_used_regs[regno])
	return 1;

      /* To save mdb requires two temporary registers.  To save mdc or
         any of the floating registers requires one temporary
         register.  If this is an interrupt routine, the temporary
         registers need to be saved as well.  These temporary registers
         are call used, so we only need deal with the case of leaf
         functions here.  */
      if (regno == PROLOGUE_TMP_REGNUM)
	{
	  if (df_regs_ever_live_p (MDB_REGNUM)
	      || df_regs_ever_live_p (MDC_REGNUM))
	    return 1;

	  for (int i = FP_FIRST_REGNUM; i <= FP_LAST_REGNUM; i++)
	    if (df_regs_ever_live_p (i))
	      return 1;
	}

      else if (regno == PROLOGUE_TMP_REGNUM + 1)
	{
	  if (df_regs_ever_live_p (MDB_REGNUM))
	    return 1;
	}
    }

  return df_regs_ever_live_p (regno) && !call_used_regs[regno];
}

/* Compute the frame size required by the function.  This function is called
   during the reload pass and also by visium_expand_prologue.  */

static int
visium_compute_frame_size (int size)
{
  const int save_area_size = visium_reg_parm_save_area_size;
  const int var_size = VISIUM_STACK_ALIGN (size);
  const int save_fp
    = frame_pointer_needed || df_regs_ever_live_p (HARD_FRAME_POINTER_REGNUM);
  const int save_lr = frame_pointer_needed || !crtl->is_leaf;
  const int lr_slot = !save_lr && df_regs_ever_live_p (long_branch_regnum);
  const int local_frame_offset
    = (save_fp + save_lr + lr_slot) * UNITS_PER_WORD;
  const int interrupt = visium_interrupt_function_p ();
  unsigned int mask[last_type];
  int reg_size1 = 0;
  int max_reg1 = 0;
  int reg_size2 = 0;
  int reg_size;
  int combine;
  int frame_size;
  int regno;

  memset (mask, 0, last_type * sizeof (unsigned int));

  /* The registers may need stacking in 2 blocks since only 32 32-bit words
     can be indexed from a given base address.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (visium_save_reg_p (interrupt, regno))
	{
	  enum reg_type reg_type = GET_REG_TYPE (regno);
	  int mask_bit = 1 << (regno - first_regno[reg_type]);
	  int nbytes = reg_type_size[reg_type];

	  if (reg_size1 + nbytes > 32 * UNITS_PER_WORD)
	    break;

	  reg_size1 += nbytes;
	  max_reg1 = regno;
	  mask[reg_type] |= mask_bit;
	}
    }

  for (regno = max_reg1 + 1; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (visium_save_reg_p (interrupt, regno))
	{
	  enum reg_type reg_type = GET_REG_TYPE (regno);
	  int mask_bit = 1 << (regno - first_regno[reg_type]);
	  int nbytes = reg_type_size[reg_type];

	  reg_size2 += nbytes;
	  mask[reg_type] |= mask_bit;
	}
    }

  reg_size = reg_size2 ? reg_size2 : reg_size1;
  combine = (local_frame_offset + var_size + reg_size) <= 32 * UNITS_PER_WORD;
  frame_size
    = local_frame_offset + var_size + reg_size2 + reg_size1 + save_area_size;

  current_frame_info.save_area_size = save_area_size;
  current_frame_info.reg_size1 = reg_size1;
  current_frame_info.max_reg1 = max_reg1;
  current_frame_info.reg_size2 = reg_size2;
  current_frame_info.var_size = var_size;
  current_frame_info.save_fp = save_fp;
  current_frame_info.save_lr = save_lr;
  current_frame_info.lr_slot = lr_slot;
  current_frame_info.combine = combine;
  current_frame_info.interrupt = interrupt;

  memcpy (current_frame_info.mask, mask, last_type * sizeof (unsigned int));

  return frame_size;
}

/* Helper function for INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET).  Define
   the offset between two registers, one to be eliminated, and the other its
   replacement, at the start of a routine.  */

int
visium_initial_elimination_offset (int from, int to ATTRIBUTE_UNUSED)
{
  const int save_fp = current_frame_info.save_fp;
  const int save_lr = current_frame_info.save_lr;
  const int lr_slot = current_frame_info.lr_slot;
  int offset;

  if (from == FRAME_POINTER_REGNUM)
    offset = (save_fp + save_lr + lr_slot) * UNITS_PER_WORD;
  else if (from == ARG_POINTER_REGNUM)
    offset = visium_compute_frame_size (get_frame_size ());
  else
    gcc_unreachable ();

  return offset;
}

/* For an interrupt handler, we may be saving call-clobbered registers.
   Say the epilogue uses these in addition to the link register.  */

int
visium_epilogue_uses (int regno)
{
  if (regno == LINK_REGNUM)
    return 1;

  if (reload_completed)
    {
      enum reg_type reg_type = GET_REG_TYPE (regno);
      int mask_bit = 1 << (regno - first_regno[reg_type]);

      return (current_frame_info.mask[reg_type] & mask_bit) != 0;
    }

  return 0;
}

/* Wrapper around emit_insn that sets RTX_FRAME_RELATED_P on the insn.  */

static rtx
emit_frame_insn (rtx x)
{
  x = emit_insn (x);
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* Allocate ALLOC bytes on the stack and save the registers LOW_REGNO to
   HIGH_REGNO at OFFSET from the stack pointer.  */

static void
visium_save_regs (int alloc, int offset, int low_regno, int high_regno)
{
  /* If this is an interrupt handler function, then mark the register
     stores as volatile.  This will prevent the instruction scheduler
     from scrambling the order of register saves.  */
  const int volatile_p = current_frame_info.interrupt;
  int regno;

  /* Allocate the stack space.  */
  emit_frame_insn (gen_addsi3_flags (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (-alloc)));

  for (regno = low_regno; regno <= high_regno; regno++)
    {
      enum reg_type reg_type = GET_REG_TYPE (regno);
      int mask_bit = 1 << (regno - first_regno[reg_type]);
      rtx insn;

      if (current_frame_info.mask[reg_type] & mask_bit)
	{
	  offset -= reg_type_size[reg_type];
	  switch (reg_type)
	    {
	    case general:
	      {
		rtx mem
		  = gen_frame_mem (SImode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_frame_insn (gen_movsi (mem, gen_rtx_REG (SImode, regno)));
	      }
	      break;

	    case mdb:
	      {
		rtx tmp = gen_rtx_REG (DImode, PROLOGUE_TMP_REGNUM);
		rtx mem
		  = gen_frame_mem (DImode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		rtx reg = gen_rtx_REG (DImode, regno);
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_insn (gen_movdi (tmp, reg));
		/* Do not generate CFI if in interrupt handler.  */
		if (volatile_p)
		  emit_insn (gen_movdi (mem, tmp));
		else
		  {
		    insn = emit_frame_insn (gen_movdi (mem, tmp));
		    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
				  gen_rtx_SET (mem, reg));
		  }
	      }
	      break;

	    case mdc:
	      {
		rtx tmp = gen_rtx_REG (SImode, PROLOGUE_TMP_REGNUM);
		rtx mem
		  = gen_frame_mem (SImode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		rtx reg = gen_rtx_REG (SImode, regno);
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_insn (gen_movsi (tmp, reg));
		insn = emit_frame_insn (gen_movsi (mem, tmp));
		add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			      gen_rtx_SET (mem, reg));
	      }
	      break;

	    case floating:
	      {
		rtx tmp = gen_rtx_REG (SFmode, PROLOGUE_TMP_REGNUM);
		rtx mem
		  = gen_frame_mem (SFmode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		rtx reg = gen_rtx_REG (SFmode, regno);
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_insn (gen_movsf (tmp, reg));
		insn = emit_frame_insn (gen_movsf (mem, tmp));
		add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			      gen_rtx_SET (mem, reg));
	      }
	      break;

	    default:
	      break;
	    }
	}
    }
}

/* This function generates the code for function entry.  */

void
visium_expand_prologue (void)
{
  const int frame_size = visium_compute_frame_size (get_frame_size ());
  const int save_area_size = current_frame_info.save_area_size;
  const int reg_size1 = current_frame_info.reg_size1;
  const int max_reg1 = current_frame_info.max_reg1;
  const int reg_size2 = current_frame_info.reg_size2;
  const int var_size = current_frame_info.var_size;
  const int save_fp = current_frame_info.save_fp;
  const int save_lr = current_frame_info.save_lr;
  const int lr_slot = current_frame_info.lr_slot;
  const int local_frame_offset
    = (save_fp + save_lr + lr_slot) * UNITS_PER_WORD;
  const int combine = current_frame_info.combine;
  int reg_size;
  int first_reg;
  int fsize;

  /* Save the frame size for future references.  */
  visium_frame_size = frame_size;

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame_size;

  /* If the registers have to be stacked in 2 blocks, stack the first one.  */
  if (reg_size2)
    {
      visium_save_regs (reg_size1 + save_area_size, reg_size1, 0, max_reg1);
      reg_size = reg_size2;
      first_reg = max_reg1 + 1;
      fsize = local_frame_offset + var_size + reg_size2;
    }
  else
    {
      reg_size = reg_size1;
      first_reg = 0;
      fsize = local_frame_offset + var_size + reg_size1 + save_area_size;
    }

  /* If we can't combine register stacking with variable allocation, partially
     allocate and stack the (remaining) registers now.  */
  if (reg_size && !combine)
    visium_save_regs (fsize - local_frame_offset - var_size, reg_size,
		      first_reg, FIRST_PSEUDO_REGISTER - 1);

  /* If we can combine register stacking with variable allocation, fully
     allocate and stack the (remaining) registers now.  */
  if (reg_size && combine)
    visium_save_regs (fsize, local_frame_offset + var_size + reg_size,
		      first_reg, FIRST_PSEUDO_REGISTER - 1);

  /* Otherwise space may still need to be allocated for the variables.  */
  else if (fsize)
    {
      const int alloc_size = reg_size ? local_frame_offset + var_size : fsize;

      if (alloc_size > 65535)
	{
	  rtx tmp = gen_rtx_REG (SImode, PROLOGUE_TMP_REGNUM), insn;
	  emit_insn (gen_movsi (tmp, GEN_INT (alloc_size)));
	  insn = emit_frame_insn (gen_subsi3_flags (stack_pointer_rtx,
						    stack_pointer_rtx,
						    tmp));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (stack_pointer_rtx,
				     gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						   GEN_INT (-alloc_size))));
	}
      else
	emit_frame_insn (gen_addsi3_flags (stack_pointer_rtx,
					   stack_pointer_rtx,
					   GEN_INT (-alloc_size)));
    }

  if (save_fp)
    emit_frame_insn (gen_movsi (gen_frame_mem (SImode, stack_pointer_rtx),
				hard_frame_pointer_rtx));

  if (frame_pointer_needed)
    emit_frame_insn (gen_stack_save ());

  if (save_lr)
    {
      rtx base_rtx, mem;

      /* Normally the frame pointer and link register get saved via
         write.l (sp),fp
         move.l  fp,sp
         write.l 1(sp),r21

         Indexing off sp rather than fp to store the link register
         avoids presenting the instruction scheduler with an initial
         pipeline hazard.  If however the frame is needed for eg.
         __builtin_return_address which needs to retrieve the saved
         value of the link register from the stack at fp + 4 then
         indexing from sp can confuse the dataflow, causing the link
         register to be retrieved before it has been saved.  */
      if (cfun->machine->frame_needed)
	base_rtx = hard_frame_pointer_rtx;
      else
	base_rtx = stack_pointer_rtx;

      mem = gen_frame_mem (SImode,
			   plus_constant (Pmode,
					  base_rtx, save_fp * UNITS_PER_WORD));
      emit_frame_insn (gen_movsi (mem, gen_rtx_REG (SImode, LINK_REGNUM)));
    }
}

static GTY(()) rtx cfa_restores;

/* Queue a REG_CFA_RESTORE note until next stack manipulation insn.  */

static void
visium_add_cfa_restore_note (rtx reg)
{
  cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
}

/* Add queued REG_CFA_RESTORE notes to INSN, if any.  */

static void
visium_add_queued_cfa_restore_notes (rtx insn)
{
  rtx last;
  if (!cfa_restores)
    return;
  for (last = cfa_restores; XEXP (last, 1); last = XEXP (last, 1))
    ;
  XEXP (last, 1) = REG_NOTES (insn);
  REG_NOTES (insn) = cfa_restores;
  cfa_restores = NULL_RTX;
}

/* Restore the registers LOW_REGNO to HIGH_REGNO from the save area at OFFSET
   from the stack pointer and pop DEALLOC bytes off the stack.  */

static void
visium_restore_regs (int dealloc, int offset, int high_regno, int low_regno)
{
  /* If this is an interrupt handler function, then mark the register
     restores as volatile.  This will prevent the instruction scheduler
     from scrambling the order of register restores.  */
  const int volatile_p = current_frame_info.interrupt;
  int r30_offset = -1;
  int regno;

  for (regno = high_regno; regno >= low_regno; --regno)
    {
      enum reg_type reg_type = GET_REG_TYPE (regno);
      int mask_bit = 1 << (regno - first_regno[reg_type]);

      if (current_frame_info.mask[reg_type] & mask_bit)
	{
	  switch (reg_type)
	    {
	    case general:
	      /* Postpone restoring the interrupted context registers
	         until last, since they need to be preceded by a dsi.  */
	      if (regno == 29)
		;
	      else if (regno == 30)
		r30_offset = offset;
	      else
		{
		  rtx mem
		    = gen_frame_mem (SImode,
				     plus_constant (Pmode,
						    stack_pointer_rtx,
						    offset));
		  rtx reg = gen_rtx_REG (SImode, regno);
		  MEM_VOLATILE_P (mem) = volatile_p;
		  emit_insn (gen_movsi (reg, mem));
		  visium_add_cfa_restore_note (reg);
		}
	      break;

	    case mdb:
	      {
		rtx tmp = gen_rtx_REG (DImode, PROLOGUE_TMP_REGNUM);
		rtx mem
		  = gen_frame_mem (DImode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		rtx reg = gen_rtx_REG (DImode, regno);
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_insn (gen_movdi (tmp, mem));
		emit_insn (gen_movdi (reg, tmp));
		/* Do not generate CFI if in interrupt handler.  */
		if (!volatile_p)
		  visium_add_cfa_restore_note (reg);
	      }
	      break;

	    case mdc:
	      {
		rtx tmp = gen_rtx_REG (SImode, PROLOGUE_TMP_REGNUM);
		rtx mem
		  = gen_frame_mem (SImode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		rtx reg = gen_rtx_REG (SImode, regno);
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_insn (gen_movsi (tmp, mem));
		emit_insn (gen_movsi (reg, tmp));
		visium_add_cfa_restore_note (reg);
	      }
	      break;

	    case floating:
	      {
		rtx tmp = gen_rtx_REG (SFmode, PROLOGUE_TMP_REGNUM);
		rtx mem
		  = gen_frame_mem (SFmode,
				   plus_constant (Pmode,
						  stack_pointer_rtx, offset));
		rtx reg = gen_rtx_REG (SFmode, regno);
		MEM_VOLATILE_P (mem) = volatile_p;
		emit_insn (gen_movsf (tmp, mem));
		emit_insn (gen_movsf (reg, tmp));
		visium_add_cfa_restore_note (reg);
	      }
	      break;

	    default:
	      break;
	    }

	  offset += reg_type_size[reg_type];
	}
    }

  /* If the interrupted context needs to be restored, precede the
     restores of r29 and r30 by a dsi.  */
  if (r30_offset >= 0)
    {
      emit_insn (gen_dsi ());
      emit_move_insn (gen_rtx_REG (SImode, 30),
		      gen_frame_mem (SImode,
				     plus_constant (Pmode,
						    stack_pointer_rtx,
						    r30_offset)));
      emit_move_insn (gen_rtx_REG (SImode, 29),
		      gen_frame_mem (SImode,
				     plus_constant (Pmode,
						    stack_pointer_rtx,
						    r30_offset + 4)));
    }

  /* Deallocate the stack space.  */
  rtx insn = emit_frame_insn (gen_stack_pop (GEN_INT (dealloc)));
  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (stack_pointer_rtx,
			     gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					   GEN_INT (dealloc))));
  visium_add_queued_cfa_restore_notes (insn);
}

/* This function generates the code for function exit.  */

void
visium_expand_epilogue (void)
{
  const int save_area_size = current_frame_info.save_area_size;
  const int reg_size1 = current_frame_info.reg_size1;
  const int max_reg1 = current_frame_info.max_reg1;
  const int reg_size2 = current_frame_info.reg_size2;
  const int var_size = current_frame_info.var_size;
  const int restore_fp = current_frame_info.save_fp;
  const int restore_lr = current_frame_info.save_lr;
  const int lr_slot = current_frame_info.lr_slot;
  const int local_frame_offset
    = (restore_fp + restore_lr + lr_slot) * UNITS_PER_WORD;
  const int combine = current_frame_info.combine;
  int reg_size;
  int last_reg;
  int fsize;

  /* Do not bother restoring the stack pointer if it hasn't been changed in
     the function since it was saved _after_ the allocation of the frame.  */
  if (!crtl->sp_is_unchanging)
    emit_insn (gen_stack_restore ());

  /* Restore the frame pointer if necessary.  The usual code would be:

       move.l  sp,fp
       read.l  fp,(sp)

     but for the MCM this constitutes a stall/hazard so it is changed to:

       move.l  sp,fp
       read.l  fp,(fp)

     if the stack pointer has actually been restored.  */
  if (restore_fp)
    {
      rtx src;

      if (TARGET_MCM && !crtl->sp_is_unchanging)
	src = gen_frame_mem (SImode, hard_frame_pointer_rtx);
      else
	src = gen_frame_mem (SImode, stack_pointer_rtx);

      rtx insn = emit_frame_insn (gen_movsi (hard_frame_pointer_rtx, src));
      add_reg_note (insn, REG_CFA_ADJUST_CFA,
		    gen_rtx_SET (stack_pointer_rtx,
				 hard_frame_pointer_rtx));
      visium_add_cfa_restore_note (hard_frame_pointer_rtx);
    }

  /* Restore the link register if necessary.  */
  if (restore_lr)
    {
      rtx mem = gen_frame_mem (SImode,
			       plus_constant (Pmode,
					      stack_pointer_rtx,
					      restore_fp * UNITS_PER_WORD));
      rtx reg = gen_rtx_REG (SImode, LINK_REGNUM);
      emit_insn (gen_movsi (reg, mem));
      visium_add_cfa_restore_note (reg);
    }

  /* If we have two blocks of registers, deal with the second one first.  */
  if (reg_size2)
    {
      reg_size = reg_size2;
      last_reg = max_reg1 + 1;
      fsize = local_frame_offset + var_size + reg_size2;
    }
  else
    {
      reg_size = reg_size1;
      last_reg = 0;
      fsize = local_frame_offset + var_size + reg_size1 + save_area_size;
    }

  /* If the variable allocation could be combined with register stacking,
     restore the (remaining) registers and fully deallocate now.  */
  if (reg_size && combine)
    visium_restore_regs (fsize, local_frame_offset + var_size,
			 FIRST_PSEUDO_REGISTER - 1, last_reg);

  /* Otherwise deallocate the variables first.  */
  else if (fsize)
    {
      const int pop_size = reg_size ? local_frame_offset + var_size : fsize;
      rtx insn;

      if (pop_size > 65535)
	{
	  rtx tmp = gen_rtx_REG (SImode, PROLOGUE_TMP_REGNUM);
	  emit_move_insn (tmp, GEN_INT (pop_size));
	  insn = emit_frame_insn (gen_stack_pop (tmp));
        }
      else
	insn = emit_frame_insn (gen_stack_pop (GEN_INT (pop_size)));
      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (stack_pointer_rtx,
				 gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					       GEN_INT (pop_size))));
      visium_add_queued_cfa_restore_notes (insn);
    }

  /* If the variable allocation couldn't be combined with register stacking,
     restore the (remaining) registers now and partially deallocate.  */
  if (reg_size && !combine)
    visium_restore_regs (fsize - local_frame_offset - var_size, 0,
			 FIRST_PSEUDO_REGISTER - 1, last_reg);

  /* If the first block of registers has yet to be restored, do it now.  */
  if (reg_size2)
    visium_restore_regs (reg_size1 + save_area_size, 0, max_reg1, 0);

  /* If this is an exception return, make the necessary stack adjustment.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_stack_pop (EH_RETURN_STACKADJ_RTX));
}

/* Return true if it is appropriate to emit `return' instructions in the
   body of a function.  */

bool
visium_can_use_return_insn_p (void)
{
  return reload_completed
	 && visium_frame_size == 0
	 && !visium_interrupt_function_p ();
}

/* Return the register class required for an intermediate register used to
   copy a register of RCLASS from/to X.  If no such intermediate register is
   required, return NO_REGS.  If more than one such intermediate register is
   required, describe the one that is closest in the copy chain to the reload
   register.  */

static reg_class_t
visium_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x,
			 reg_class_t rclass,
			 machine_mode mode ATTRIBUTE_UNUSED,
			 secondary_reload_info *sri ATTRIBUTE_UNUSED)
{
  int regno = true_regnum (x);

  /* For MDB, MDC and FP_REGS, a general register is needed for a move to
     or from memory. */
  if (regno == -1 && (rclass == MDB || rclass == MDC || rclass == FP_REGS))
    return GENERAL_REGS;

  /* Moves between MDB, MDC and FP_REGS also require a general register. */
  else if (((regno == R_MDB || regno == R_MDC) && rclass == FP_REGS)
      || (FP_REGISTER_P (regno) && (rclass == MDB || rclass == MDC)))
    return GENERAL_REGS;

  /* Finally an (unlikely ?) move between MDB and MDC needs a general reg. */
  else if ((regno == R_MDB && rclass == MDC)
	   || (rclass == MDB && regno == R_MDC))
    return GENERAL_REGS;

  return NO_REGS;
}

/* Return true if pseudos that have been assigned to registers of RCLASS
   would likely be spilled because registers of RCLASS are needed for
   spill registers.  */

static bool
visium_class_likely_spilled_p (reg_class_t rclass ATTRIBUTE_UNUSED)
{
  /* Return false for classes R1, R2 and R3, which are intended to be used
     only in the source code in conjunction with block move instructions.  */
  return false;
}

/* Return the register number if OP is a REG or a SUBREG of a REG, and
   INVALID_REGNUM in all the other cases.  */

unsigned int
reg_or_subreg_regno (rtx op)
{
  unsigned int regno;

  if (GET_CODE (op) == REG)
    regno = REGNO (op);
  else if (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
    {
      if (REGNO (SUBREG_REG (op)) < FIRST_PSEUDO_REGISTER)
	regno = subreg_regno (op);
      else
	regno = REGNO (SUBREG_REG (op));
    }
  else
    regno = INVALID_REGNUM;

  return regno;
}

#include "gt-visium.h"
