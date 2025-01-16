/* Subroutines used for code generation on the Lattice Mico32 architecture.
   Contributed by Jon Beniston <jon@beniston.com>

   Copyright (C) 2009-2025 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "output.h"
#include "calls.h"
#include "alias.h"
#include "explow.h"
#include "expr.h"
#include "tm-constrs.h"
#include "builtins.h"
#include "langhooks.h"
#include "stor-layout.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimplify.h"

/* This file should be included last.  */
#include "target-def.h"

struct lm32_frame_info
{
  HOST_WIDE_INT total_size;	/* number of bytes of entire frame.  */
  HOST_WIDE_INT callee_size;	/* number of bytes to save callee saves.  */
  HOST_WIDE_INT pretend_size;	/* number of bytes we pretend caller did.  */
  HOST_WIDE_INT args_size;	/* number of bytes for outgoing arguments.  */
  HOST_WIDE_INT locals_size;	/* number of bytes for local variables.  */
  unsigned int reg_save_mask;	/* mask of saved registers.  */
};

/* Prototypes for static functions.  */
static rtx emit_add (rtx dest, rtx src0, rtx src1);
static void expand_save_restore (struct lm32_frame_info *info, int op);
static void stack_adjust (HOST_WIDE_INT amount);
static bool lm32_in_small_data_p (const_tree);
static void lm32_setup_incoming_varargs (cumulative_args_t cum,
					 const function_arg_info &,
					 int *pretend_size, int no_rtl);
static tree lm32_build_builtin_va_list (void);
static void lm32_builtin_va_start (tree valist, rtx nextarg);
static tree lm32_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
				       gimple_seq *post_p);
static bool lm32_rtx_costs (rtx x, machine_mode mode, int outer_code, int opno,
			    int *total, bool speed);
static bool lm32_can_eliminate (const int, const int);
static bool lm32_legitimate_address_p (machine_mode mode, rtx x, bool strict,
				       code_helper = ERROR_MARK);
static HOST_WIDE_INT lm32_compute_frame_size (int size);
static void lm32_option_override (void);
static rtx lm32_function_arg (cumulative_args_t, const function_arg_info &);
static void lm32_function_arg_advance (cumulative_args_t cum,
				       const function_arg_info &);
static bool lm32_hard_regno_mode_ok (unsigned int, machine_mode);
static bool lm32_modes_tieable_p (machine_mode, machine_mode);
static HOST_WIDE_INT lm32_starting_frame_offset (void);

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE lm32_option_override
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS lm32_rtx_costs
#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P lm32_in_small_data_p
#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS lm32_setup_incoming_varargs
#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST lm32_build_builtin_va_list
#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START lm32_builtin_va_start
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR lm32_gimplify_va_arg_expr

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG lm32_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE lm32_function_arg_advance
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -0x8000
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 0x7fff
#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE lm32_can_eliminate
#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false
#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P lm32_legitimate_address_p
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK lm32_hard_regno_mode_ok
#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P lm32_modes_tieable_p

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

#undef TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET lm32_starting_frame_offset

struct gcc_target targetm = TARGET_INITIALIZER;

/* Current frame information calculated by lm32_compute_frame_size.  */
static struct lm32_frame_info current_frame_info;

/* Return non-zero if the given return type should be returned in memory.  */

int
lm32_return_in_memory (tree type)
{
  HOST_WIDE_INT size;

  if (!AGGREGATE_TYPE_P (type))
    {
      /* All simple types are returned in registers.  */
      return 0;
    }

  size = int_size_in_bytes (type);
  if (size >= 0 && size <= UNITS_PER_WORD)
    {
      /* If it can fit in one register.  */
      return 0;
    }

  return 1;
}

/* Generate an emit a word sized add instruction.  */

static rtx
emit_add (rtx dest, rtx src0, rtx src1)
{
  rtx insn;
  insn = emit_insn (gen_addsi3 (dest, src0, src1));
  return insn;
}

/* Generate the code to compare (and possibly branch) two integer values
   TEST_CODE is the comparison code we are trying to emulate
     (or implement directly)
   RESULT is where to store the result of the comparison,
     or null to emit a branch
   CMP0 CMP1 are the two comparison operands
   DESTINATION is the destination of the branch, or null to only compare
   */

static void
gen_int_relational (enum rtx_code code,
		    rtx result,
		    rtx cmp0,
		    rtx cmp1,
		    rtx destination)
{
  machine_mode mode;
  int branch_p;

  mode = GET_MODE (cmp0);
  if (mode == VOIDmode)
    mode = GET_MODE (cmp1);

  /* Is this a branch or compare.  */
  branch_p = (destination != 0);

  /* Instruction set doesn't support LE or LT, so swap operands and use
     GE, GT.  */
  switch (code)
    {
    case LE:
    case LT:
    case LEU:
    case LTU:
      {
	rtx temp;

	code = swap_condition (code);
	temp = cmp0;
	cmp0 = cmp1;
	cmp1 = temp;
	break;
      }
    default:
      break;
    }

  if (branch_p)
    {
      rtx insn, cond, label;

      /* Operands must be in registers.  */
      if (!register_operand (cmp0, mode))
	cmp0 = force_reg (mode, cmp0);
      if (!register_operand (cmp1, mode))
	cmp1 = force_reg (mode, cmp1);

      /* Generate conditional branch instruction.  */
      cond = gen_rtx_fmt_ee (code, mode, cmp0, cmp1);
      label = gen_rtx_LABEL_REF (VOIDmode, destination);
      insn = gen_rtx_SET (pc_rtx, gen_rtx_IF_THEN_ELSE (VOIDmode,
							cond, label, pc_rtx));
      emit_jump_insn (insn);
    }
  else
    {
      /* We can't have const_ints in cmp0, other than 0.  */
      if ((GET_CODE (cmp0) == CONST_INT) && (INTVAL (cmp0) != 0))
	cmp0 = force_reg (mode, cmp0);

      /* If the comparison is against an int not in legal range
         move it into a register.  */
      if (GET_CODE (cmp1) == CONST_INT)
	{
	  switch (code)
	    {
	    case EQ:
	    case NE:
	    case LE:
	    case LT:
	    case GE:
	    case GT:
	      if (!satisfies_constraint_K (cmp1))
		cmp1 = force_reg (mode, cmp1);
	      break;
	    case LEU:
	    case LTU:
	    case GEU:
	    case GTU:
	      if (!satisfies_constraint_L (cmp1))
		cmp1 = force_reg (mode, cmp1);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}

      /* Generate compare instruction.  */
      emit_move_insn (result, gen_rtx_fmt_ee (code, mode, cmp0, cmp1));
    }
}

/* Try performing the comparison in OPERANDS[1], whose arms are OPERANDS[2]
   and OPERAND[3].  Store the result in OPERANDS[0].  */

void
lm32_expand_scc (rtx operands[])
{
  rtx target = operands[0];
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[2];
  rtx op1 = operands[3];

  gen_int_relational (code, target, op0, op1, NULL_RTX);
}

/* Compare OPERANDS[1] with OPERANDS[2] using comparison code
   CODE and jump to OPERANDS[3] if the condition holds.  */

void
lm32_expand_conditional_branch (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx destination = operands[3];

  gen_int_relational (code, NULL_RTX, op0, op1, destination);
}

/* Generate and emit RTL to save or restore callee save registers.  */
static void
expand_save_restore (struct lm32_frame_info *info, int op)
{
  unsigned int reg_save_mask = info->reg_save_mask;
  int regno;
  HOST_WIDE_INT offset;
  rtx insn;

  /* Callee saves are below locals and above outgoing arguments.  */
  offset = info->args_size + info->callee_size;
  for (regno = 0; regno <= 31; regno++)
    {
      if ((reg_save_mask & (1 << regno)) != 0)
	{
	  rtx offset_rtx;
	  rtx mem;

	  offset_rtx = GEN_INT (offset);
	  if (satisfies_constraint_K (offset_rtx))
	    {
              mem = gen_rtx_MEM (word_mode,
                                 gen_rtx_PLUS (Pmode,
                                               stack_pointer_rtx,
                                               offset_rtx));
            }
          else
            {
              /* r10 is caller saved so it can be used as a temp reg.  */
              rtx r10;

              r10 = gen_rtx_REG (word_mode, 10);
              insn = emit_move_insn (r10, offset_rtx);
              if (op == 0)
                RTX_FRAME_RELATED_P (insn) = 1;
              insn = emit_add (r10, r10, stack_pointer_rtx);
              if (op == 0)
                RTX_FRAME_RELATED_P (insn) = 1;
              mem = gen_rtx_MEM (word_mode, r10);
            }

	  if (op == 0)
	    insn = emit_move_insn (mem, gen_rtx_REG (word_mode, regno));
	  else
	    insn = emit_move_insn (gen_rtx_REG (word_mode, regno), mem);

	  /* only prologue instructions which set the sp fp or save a
	     register should be marked as frame related.  */
	  if (op == 0)
	    RTX_FRAME_RELATED_P (insn) = 1;
	  offset -= UNITS_PER_WORD;
	}
    }
}

static void
stack_adjust (HOST_WIDE_INT amount)
{
  rtx insn;

  if (!IN_RANGE (amount, -32776, 32768))
    {
      /* r10 is caller saved so it can be used as a temp reg.  */
      rtx r10;
      r10 = gen_rtx_REG (word_mode, 10);
      insn = emit_move_insn (r10, GEN_INT (amount));
      if (amount < 0)
	RTX_FRAME_RELATED_P (insn) = 1;
      insn = emit_add (stack_pointer_rtx, stack_pointer_rtx, r10);
      if (amount < 0)
	RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      insn = emit_add (stack_pointer_rtx,
		       stack_pointer_rtx, GEN_INT (amount));
      if (amount < 0)
	RTX_FRAME_RELATED_P (insn) = 1;
    }
}


/* Create and emit instructions for a functions prologue.  */
void
lm32_expand_prologue (void)
{
  rtx insn;

  lm32_compute_frame_size (get_frame_size ());

  if (current_frame_info.total_size > 0)
    {
      /* Add space on stack new frame.  */
      stack_adjust (-current_frame_info.total_size);

      /* Save callee save registers.  */
      if (current_frame_info.reg_save_mask != 0)
	expand_save_restore (&current_frame_info, 0);

      /* Setup frame pointer if it's needed.  */
      if (frame_pointer_needed == 1)
	{
	  /* Move sp to fp.  */
	  insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  /* Add offset - Don't use total_size, as that includes pretend_size,
             which isn't part of this frame?  */
	  insn = emit_add (frame_pointer_rtx,
			   frame_pointer_rtx,
			   GEN_INT (current_frame_info.args_size +
				    current_frame_info.callee_size +
				    current_frame_info.locals_size));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* Prevent prologue from being scheduled into function body.  */
      emit_insn (gen_blockage ());
    }
}

/* Create an emit instructions for a functions epilogue.  */
void
lm32_expand_epilogue (void)
{
  rtx ra_rtx = gen_rtx_REG (Pmode, RA_REGNUM);

  lm32_compute_frame_size (get_frame_size ());

  if (current_frame_info.total_size > 0)
    {
      /* Prevent stack code from being reordered.  */
      emit_insn (gen_blockage ());

      /* Restore callee save registers.  */
      if (current_frame_info.reg_save_mask != 0)
	expand_save_restore (&current_frame_info, 1);

      /* Deallocate stack.  */
      stack_adjust (current_frame_info.total_size);

      /* Return to calling function.  */
      emit_jump_insn (gen_return_internal (ra_rtx));
    }
  else
    {
      /* Return to calling function.  */
      emit_jump_insn (gen_return_internal (ra_rtx));
    }
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.  */
static HOST_WIDE_INT
lm32_compute_frame_size (int size)
{
  int regno;
  HOST_WIDE_INT total_size, locals_size, args_size, pretend_size, callee_size;
  unsigned int reg_save_mask;

  locals_size = size;
  args_size = crtl->outgoing_args_size;
  pretend_size = crtl->args.pretend_args_size;
  callee_size = 0;
  reg_save_mask = 0;

  /* Build mask that actually determines which regsiters we save
     and calculate size required to store them in the stack.  */
  for (regno = 1; regno < SP_REGNUM; regno++)
    {
      if (df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
	{
	  reg_save_mask |= 1 << regno;
	  callee_size += UNITS_PER_WORD;
	}
    }
  if (df_regs_ever_live_p (RA_REGNUM) || ! crtl->is_leaf
      || !optimize)
    {
      reg_save_mask |= 1 << RA_REGNUM;
      callee_size += UNITS_PER_WORD;
    }
  if (!(reg_save_mask & (1 << FP_REGNUM)) && frame_pointer_needed)
    {
      reg_save_mask |= 1 << FP_REGNUM;
      callee_size += UNITS_PER_WORD;
    }

  /* Compute total frame size.  */
  total_size = pretend_size + args_size + locals_size + callee_size;

  /* Align frame to appropriate boundary.  */
  total_size = (total_size + 3) & ~3;

  /* Save computed information.  */
  current_frame_info.total_size = total_size;
  current_frame_info.callee_size = callee_size;
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.locals_size = locals_size;
  current_frame_info.args_size = args_size;
  current_frame_info.reg_save_mask = reg_save_mask;

  return total_size;
}

void
lm32_print_operand (FILE * file, rtx op, int letter)
{
  enum rtx_code code;

  code = GET_CODE (op);

  if (code == SIGN_EXTEND)
    op = XEXP (op, 0), code = GET_CODE (op);
  else if (code == REG || code == SUBREG)
    {
      int regnum;

      if (code == REG)
	regnum = REGNO (op);
      else
	regnum = true_regnum (op);

      fprintf (file, "%s", reg_names[regnum]);
    }
  else if (code == HIGH)
    output_addr_const (file, XEXP (op, 0));
  else if (code == MEM)
    output_address (GET_MODE (op), XEXP (op, 0));
  else if (letter == 'z' && GET_CODE (op) == CONST_INT && INTVAL (op) == 0)
    fprintf (file, "%s", reg_names[0]);
  else if (GET_CODE (op) == CONST_DOUBLE)
    {
      if ((CONST_DOUBLE_LOW (op) != 0) || (CONST_DOUBLE_HIGH (op) != 0))
	output_operand_lossage ("only 0.0 can be loaded as an immediate");
      else
	fprintf (file, "0");
    }
  else if (code == EQ)
    fprintf (file, "e  ");
  else if (code == NE)
    fprintf (file, "ne ");
  else if (code == GT)
    fprintf (file, "g  ");
  else if (code == GTU)
    fprintf (file, "gu ");
  else if (code == LT)
    fprintf (file, "l  ");
  else if (code == LTU)
    fprintf (file, "lu ");
  else if (code == GE)
    fprintf (file, "ge ");
  else if (code == GEU)
    fprintf (file, "geu");
  else if (code == LE)
    fprintf (file, "le ");
  else if (code == LEU)
    fprintf (file, "leu");
  else
    output_addr_const (file, op);
}

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */

void
lm32_print_operand_address (FILE * file, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "(%s+0)", reg_names[REGNO (addr)]);
      break;

    case MEM:
      output_address (VOIDmode, XEXP (addr, 0));
      break;

    case PLUS:
      {
	rtx arg0 = XEXP (addr, 0);
	rtx arg1 = XEXP (addr, 1);

	if (GET_CODE (arg0) == REG && CONSTANT_P (arg1))
	  {
	    if (GET_CODE (arg1) == CONST_INT)
	      fprintf (file, "(%s+%ld)", reg_names[REGNO (arg0)],
		       INTVAL (arg1));
	    else
	      {
		fprintf (file, "(%s+", reg_names[REGNO (arg0)]);
		output_addr_const (file, arg1);
		fprintf (file, ")");
	      }
	  }
	else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))
	  output_addr_const (file, addr);
	else
	  fatal_insn ("bad operand", addr);
      }
      break;

    case SYMBOL_REF:
      if (SYMBOL_REF_SMALL_P (addr))
	{
	  fprintf (file, "gp(");
	  output_addr_const (file, addr);
	  fprintf (file, ")");
	}
      else
	fatal_insn ("can't use non gp relative absolute address", addr);
      break;

    default:
      fatal_insn ("invalid addressing mode", addr);
      break;
    }
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   ARG is a description of the argument.  */

static rtx
lm32_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (arg.end_marker_p ())
    /* Compute operand 2 of the call insn.  */
    return GEN_INT (0);

  if (targetm.calls.must_pass_in_stack (arg))
    return NULL_RTX;

  if (*cum + LM32_NUM_REGS2 (arg.mode, arg.type) > LM32_NUM_ARG_REGS)
    return NULL_RTX;

  return gen_rtx_REG (arg.mode, *cum + LM32_FIRST_ARG_REG);
}

static void
lm32_function_arg_advance (cumulative_args_t cum,
			   const function_arg_info &arg)
{
  *get_cumulative_args (cum) += LM32_NUM_REGS2 (arg.mode, arg.type);
}

HOST_WIDE_INT
lm32_compute_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset = 0;

  switch (from)
    {
    case ARG_POINTER_REGNUM:
      switch (to)
	{
	case FRAME_POINTER_REGNUM:
	  offset = 0;
	  break;
	case STACK_POINTER_REGNUM:
	  offset =
	    lm32_compute_frame_size (get_frame_size ()) -
	    current_frame_info.pretend_size;
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    default:
      gcc_unreachable ();
    }

  return offset;
}

static void
lm32_setup_incoming_varargs (cumulative_args_t cum_v,
			     const function_arg_info &arg,
			     int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS next_cum = *get_cumulative_args (cum_v);
  int first_anon_arg;
  tree fntype;

  fntype = TREE_TYPE (current_function_decl);

  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    lm32_function_arg_advance (pack_cumulative_args (&next_cum), arg);

  if (stdarg_p (fntype))
    first_anon_arg = next_cum + LM32_FIRST_ARG_REG;
  else
    {
      /* this is the common case, we have been passed details setup
	 for the last named argument, we want to skip over the
	 registers, if any used in passing this named parameter in
	 order to determine which is the first registers used to pass
	 anonymous arguments.  */
      int size = arg.promoted_size_in_bytes ();

      first_anon_arg =
	next_cum + LM32_FIRST_ARG_REG +
	((size + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
    }

  if (FUNCTION_ARG_REGNO_P (first_anon_arg))
    {
      int size = LM32_FIRST_ARG_REG + LM32_NUM_ARG_REGS - first_anon_arg;

      if (!no_rtl)
	{
	  rtx regblock
	    = gen_rtx_MEM (BLKmode,
			   plus_constant (Pmode, arg_pointer_rtx,
					  FIRST_PARM_OFFSET (0)));
	  move_block_from_reg (first_anon_arg, regblock, size);
	}

      *pretend_size = size * UNITS_PER_WORD;
    }
}
/* This is the "struct __va_list".  */

static GTY(()) tree va_list_type;

/* Implement TARGET_BUILD_BUILTIN_VA_LIST.  */

static tree
lm32_build_builtin_va_list (void)
{
  /* We keep one pointer and a count

     The pointer is the regular void *

     The count tracks how many registers arguments
     remain. When that goes to zero, we have to skip
     over the reserved space that was the top of the
     stack at function entry

   */
  tree va_list_name;
  tree ap_field;
  tree ap_reg_field;

  va_list_type = lang_hooks.types.make_type (RECORD_TYPE);
  /* Name it */
  va_list_name = build_decl (BUILTINS_LOCATION,
			     TYPE_DECL,
			     get_identifier ("__va_list"),
			     va_list_type);

  DECL_ARTIFICIAL (va_list_name) = 1;
  TYPE_NAME (va_list_type) = va_list_name;
  TYPE_STUB_DECL (va_list_type) = va_list_name;

  ap_field = build_decl (BUILTINS_LOCATION,
			 FIELD_DECL,
			 get_identifier("__ap"),
			 ptr_type_node);
  DECL_ARTIFICIAL (ap_field) = 1;
  DECL_FIELD_CONTEXT (ap_field) = va_list_type;
  TYPE_FIELDS (va_list_type) = ap_field;

  ap_reg_field = build_decl(BUILTINS_LOCATION,
			    FIELD_DECL,
			    get_identifier("__ap_reg"),
			    ptr_type_node);
  DECL_ARTIFICIAL (ap_reg_field) = 1;
  DECL_FIELD_CONTEXT (ap_reg_field) = va_list_type;
  DECL_CHAIN (ap_field) = ap_reg_field;

  layout_type (va_list_type);

  return va_list_type;
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */

static void
lm32_builtin_va_start (tree valist, rtx nextarg)
{
  const CUMULATIVE_ARGS *cum;
  tree ap_field, ap_reg_field;
  tree ap, ap_reg;
  tree t;
  int pretend_args_size = crtl->args.pretend_args_size;
  cum = &crtl->args.info;

  ap_field = TYPE_FIELDS(TREE_TYPE (valist));
  ap = build3 (COMPONENT_REF, TREE_TYPE (ap_field), valist,
	       ap_field, NULL_TREE);

  std_expand_builtin_va_start (ap, nextarg);

  ap_reg_field = DECL_CHAIN(ap_field);
  ap_reg = build3 (COMPONENT_REF, TREE_TYPE (ap_reg_field), valist,
		   ap_reg_field, NULL_TREE);

  /* Emit code to initialize __ap_reg */

  rtx last_reg_arg = expand_binop (ptr_mode, add_optab,
				   crtl->args.internal_arg_pointer,
				   gen_int_mode (pretend_args_size, Pmode),
				   NULL_RTX, 0, OPTAB_LIB_WIDEN);

  rtx ap_reg_r = expand_expr (ap_reg, NULL_RTX, VOIDmode, EXPAND_WRITE);
  convert_move (ap_reg_r, last_reg_arg, 0);
}

#ifndef PAD_VARARGS_DOWN
#define PAD_VARARGS_DOWN BYTES_BIG_ENDIAN
#endif

/*
 * This was copied from "standard" implementation of va_arg, and then
 * handling for overflow of the register paramters added
 */

static tree
lm32_std_gimplify_va_arg_expr (tree valist, tree ap_reg, tree type, gimple_seq *pre_p,
			       gimple_seq *post_p)
{
  tree addr, t, type_size, rounded_size, valist_tmp;
  unsigned HOST_WIDE_INT align, boundary;
  bool indirect;

  /* All of the alignment and movement below is for args-grow-up machines.
     As of 2004, there are only 3 ARGS_GROW_DOWNWARD targets, and they all
     implement their own specialized gimplify_va_arg_expr routines.  */
  if (ARGS_GROW_DOWNWARD)
    gcc_unreachable ();

  indirect = pass_va_arg_by_reference (type);
  if (indirect)
    type = build_pointer_type (type);

  if (targetm.calls.split_complex_arg
      && TREE_CODE (type) == COMPLEX_TYPE
      && targetm.calls.split_complex_arg (type))
    {
      tree real_part, imag_part;

      real_part = std_gimplify_va_arg_expr (valist,
					    TREE_TYPE (type), pre_p, NULL);
      real_part = get_initialized_tmp_var (real_part, pre_p);

      imag_part = std_gimplify_va_arg_expr (unshare_expr (valist),
					    TREE_TYPE (type), pre_p, NULL);
      imag_part = get_initialized_tmp_var (imag_part, pre_p);

      return build2 (COMPLEX_EXPR, type, real_part, imag_part);
   }

  align = PARM_BOUNDARY / BITS_PER_UNIT;
  boundary = targetm.calls.function_arg_boundary (TYPE_MODE (type), type);

  /* When we align parameter on stack for caller, if the parameter
     alignment is beyond MAX_SUPPORTED_STACK_ALIGNMENT, it will be
     aligned at MAX_SUPPORTED_STACK_ALIGNMENT.  We will match callee
     here with caller.  */
  if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
    boundary = MAX_SUPPORTED_STACK_ALIGNMENT;

  boundary /= BITS_PER_UNIT;

  /* Hoist the valist value into a temporary for the moment.  */
  valist_tmp = get_initialized_tmp_var (valist, pre_p);

  /* va_list pointer is aligned to PARM_BOUNDARY.  If argument actually
     requires greater alignment, we must perform dynamic alignment.  */
  if (boundary > align
      && !TYPE_EMPTY_P (type)
      && !integer_zerop (TYPE_SIZE (type)))
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build_pointer_plus_hwi (valist_tmp, boundary - 1));
      gimplify_and_add (t, pre_p);

      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build2 (BIT_AND_EXPR, TREE_TYPE (valist),
			       valist_tmp,
			       build_int_cst (TREE_TYPE (valist), -boundary)));
      gimplify_and_add (t, pre_p);
    }
  else
    boundary = align;

  /* If the actual alignment is less than the alignment of the type,
     adjust the type accordingly so that we don't assume strict alignment
     when dereferencing the pointer.  */
  boundary *= BITS_PER_UNIT;
  if (boundary < TYPE_ALIGN (type))
    {
      type = build_variant_type_copy (type);
      SET_TYPE_ALIGN (type, boundary);
    }

  /* Compute the rounded size of the type.  */
  type_size = arg_size_in_bytes (type);
  rounded_size = round_up (type_size, align);

  /* Reduce rounded_size so it's sharable with the postqueue.  */
  gimplify_expr (&rounded_size, pre_p, post_p, is_gimple_val, fb_rvalue);

  /*
   * Check for a large parameter which didn't fit in the remaining registers
   * and got pushed off to the stack instead
   */
  if (int_size_in_bytes(type) > UNITS_PER_WORD) {

    /* Hoist the ap_reg value into a temporary for the moment.  */
    tree ap_reg_tmp = get_initialized_tmp_var (ap_reg, pre_p);

    t = fold_build2_loc (input_location, TRUTH_AND_EXPR,
			 boolean_type_node,
			 fold_build2_loc (input_location, LT_EXPR, boolean_type_node,
					  valist_tmp, ap_reg_tmp),
			 fold_build2_loc (input_location, GT_EXPR, boolean_type_node,
					  fold_build_pointer_plus (valist_tmp, rounded_size), ap_reg_tmp));

    t = fold_build3 (COND_EXPR, TREE_TYPE(valist), t, ap_reg_tmp, valist_tmp);

    t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp, t);

    gimplify_and_add (t, pre_p);
  }

  /* Get AP.  */
  addr = valist_tmp;
  if (PAD_VARARGS_DOWN && !integer_zerop (rounded_size))
    {
      /* Small args are padded downward.  */
      t = fold_build2_loc (input_location, GT_EXPR, sizetype,
		       rounded_size, size_int (align));
      t = fold_build3 (COND_EXPR, sizetype, t, size_zero_node,
		       size_binop (MINUS_EXPR, rounded_size, type_size));
      addr = fold_build_pointer_plus (addr, t);
    }

  /* Compute new value for AP.  */
  t = fold_build_pointer_plus (valist_tmp, rounded_size);
  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
  gimplify_and_add (t, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (indirect)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}

/* Return an expression of type "void *" pointing to the next
   available argument in a variable-argument list.  VALIST is the
   user-level va_list object, of type __builtin_va_list.  */
/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  */
static tree
lm32_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			  gimple_seq *post_p)
{
  tree ap_field = TYPE_FIELDS(TREE_TYPE (valist));
  tree ap_reg_field = DECL_CHAIN(ap_field);
  tree ap = build3 (COMPONENT_REF, TREE_TYPE (ap_field), valist,
		    ap_field, NULL_TREE);
  tree ap_reg = build3 (COMPONENT_REF, TREE_TYPE (ap_reg_field), valist,
			ap_reg_field, NULL_TREE);

  return lm32_std_gimplify_va_arg_expr(ap, ap_reg, type, pre_p, post_p);
}

/* Override command line options.  */
static void
lm32_option_override (void)
{
  /* We must have sign-extend enabled if barrel-shift isn't.  */
  if (!TARGET_BARREL_SHIFT_ENABLED && !TARGET_SIGN_EXTEND_ENABLED)
    target_flags |= MASK_SIGN_EXTEND_ENABLED;
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */
int
lm32_can_use_return (void)
{
  if (!reload_completed)
    return 0;

  if (df_regs_ever_live_p (RA_REGNUM) || crtl->profile)
    return 0;

  if (lm32_compute_frame_size (get_frame_size ()) != 0)
    return 0;

  return 1;
}

/* Support function to determine the return address of the function
   'count' frames back up the stack.  */
rtx
lm32_return_addr_rtx (int count, rtx frame)
{
  rtx r;
  if (count == 0)
    {
      if (!df_regs_ever_live_p (RA_REGNUM))
	r = gen_rtx_REG (Pmode, RA_REGNUM);
      else
	{
	  r = gen_rtx_MEM (Pmode,
			   gen_rtx_PLUS (Pmode, frame,
					 GEN_INT (-2 * UNITS_PER_WORD)));
	  set_mem_alias_set (r, get_frame_alias_set ());
	}
    }
  else if (flag_omit_frame_pointer)
    r = NULL_RTX;
  else
    {
      r = gen_rtx_MEM (Pmode,
		       gen_rtx_PLUS (Pmode, frame,
				     GEN_INT (-2 * UNITS_PER_WORD)));
      set_mem_alias_set (r, get_frame_alias_set ());
    }
  return r;
}

/* Return true if EXP should be placed in the small data section.  */

static bool
lm32_in_small_data_p (const_tree exp)
{
  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (exp) == STRING_CST)
    return false;

  /* Functions are never in the small data area.  Duh.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    return false;

  if (VAR_P (exp) && DECL_SECTION_NAME (exp))
    {
      const char *section = DECL_SECTION_NAME (exp);
      if (strcmp (section, ".sdata") == 0 || strcmp (section, ".sbss") == 0)
	return true;
    }
  else
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

      /* If this is an incomplete type with size 0, then we can't put it
         in sdata because it might be too big when completed.  */
      if (size > 0 && size <= g_switch_value)
	return true;
    }

  return false;
}

/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
lm32_block_move_inline (rtx dest, rtx src, HOST_WIDE_INT length,
			HOST_WIDE_INT alignment)
{
  HOST_WIDE_INT offset, delta;
  unsigned HOST_WIDE_INT bits;
  int i;
  machine_mode mode;
  rtx *regs;

  /* Work out how many bits to move at a time.  */
  switch (alignment)
    {
    case 1:
      bits = 8;
      break;
    case 2:
      bits = 16;
      break;
    default:
      bits = 32;
      break;
    }

  mode = int_mode_for_size (bits, 0).require ();
  delta = bits / BITS_PER_UNIT;

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, length / delta);

  /* Load as many BITS-sized chunks as possible.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      emit_move_insn (regs[i], adjust_address (src, mode, offset));
    }

  /* Copy the chunks to the destination.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    emit_move_insn (adjust_address (dest, mode, offset), regs[i]);

  /* Mop up any left-over bytes.  */
  if (offset < length)
    {
      src = adjust_address (src, BLKmode, offset);
      dest = adjust_address (dest, BLKmode, offset);
      move_by_pieces (dest, src, length - offset,
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), RETURN_BEGIN);
    }
}

/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

int
lm32_expand_block_move (rtx * operands)
{
  if ((GET_CODE (operands[2]) == CONST_INT) && (INTVAL (operands[2]) <= 32))
    {
      lm32_block_move_inline (operands[0], operands[1], INTVAL (operands[2]),
			      INTVAL (operands[3]));
      return 1;
    }
  return 0;
}

/* Return TRUE if X references a SYMBOL_REF or LABEL_REF whose symbol
   isn't protected by a PIC unspec.  */
int
nonpic_symbol_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF
      || GET_CODE (x) == PC)
    return 1;

  /* We don't want to look into the possible MEM location of a
     CONST_DOUBLE, since we're not going to use it, in general.  */
  if (GET_CODE (x) == CONST_DOUBLE)
    return 0;

  if (GET_CODE (x) == UNSPEC)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (nonpic_symbol_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && nonpic_symbol_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
lm32_rtx_costs (rtx x, machine_mode mode, int outer_code,
		int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  int code = GET_CODE (x);
  bool small_mode;

  const int arithmetic_latency = 1;
  const int shift_latency = 1;
  const int compare_latency = 2;
  const int multiply_latency = 3;
  const int load_latency = 3;
  const int libcall_size_cost = 5;

  /* Determine if we can handle the given mode size in a single instruction.  */
  small_mode = (mode == QImode) || (mode == HImode) || (mode == SImode);

  switch (code)
    {

    case PLUS:
    case MINUS:
    case AND:
    case IOR:
    case XOR:
    case NOT:
    case NEG:
      if (!speed)
	*total = COSTS_N_INSNS (LM32_NUM_REGS (mode));
      else
	*total =
	  COSTS_N_INSNS (arithmetic_latency + (LM32_NUM_REGS (mode) - 1));
      break;

    case COMPARE:
      if (small_mode)
	{
	  if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (compare_latency);
	}
      else
	{
	  /* FIXME. Guessing here.  */
	  *total = COSTS_N_INSNS (LM32_NUM_REGS (mode) * (2 + 3) / 2);
	}
      break;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (TARGET_BARREL_SHIFT_ENABLED && small_mode)
	{
	  if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (shift_latency);
	}
      else if (TARGET_BARREL_SHIFT_ENABLED)
	{
	  /* FIXME: Guessing here.  */
	  *total = COSTS_N_INSNS (LM32_NUM_REGS (mode) * 4);
	}
      else if (small_mode && GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  *total = COSTS_N_INSNS (INTVAL (XEXP (x, 1)));
	}
      else
	{
	  /* Libcall.  */
	  if (!speed)
	    *total = COSTS_N_INSNS (libcall_size_cost);
	  else
	    *total = COSTS_N_INSNS (100);
	}
      break;

    case MULT:
      if (TARGET_MULTIPLY_ENABLED && small_mode)
	{
	  if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (multiply_latency);
	}
      else
	{
	  /* Libcall.  */
	  if (!speed)
	    *total = COSTS_N_INSNS (libcall_size_cost);
	  else
	    *total = COSTS_N_INSNS (100);
	}
      break;

    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      if (TARGET_DIVIDE_ENABLED && small_mode)
	{
	  if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else
	    {
	      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
		{
		  int cycles = 0;
		  unsigned HOST_WIDE_INT i = INTVAL (XEXP (x, 1));

		  while (i)
		    {
		      i >>= 2;
		      cycles++;
		    }
		  if (IN_RANGE (i, 0, 65536))
		    *total = COSTS_N_INSNS (1 + 1 + cycles);
		  else
		    *total = COSTS_N_INSNS (2 + 1 + cycles);
		  return true;
		}
	      else if (GET_CODE (XEXP (x, 1)) == REG)
		{
		  *total = COSTS_N_INSNS (1 + GET_MODE_SIZE (mode) / 2);
		  return true;
		}
	      else
		{
		  *total = COSTS_N_INSNS (1 + GET_MODE_SIZE (mode) / 2);
		  return false;
		}
	    }
	}
      else
	{
	  /* Libcall.  */
	  if (!speed)
	    *total = COSTS_N_INSNS (libcall_size_cost);
	  else
	    *total = COSTS_N_INSNS (100);
	}
      break;

    case HIGH:
    case LO_SUM:
      if (!speed)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (arithmetic_latency);
      break;

    case ZERO_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	*total = COSTS_N_INSNS (0);
      else if (small_mode)
	{
	  if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (arithmetic_latency);
	}
      else
	*total = COSTS_N_INSNS (LM32_NUM_REGS (mode) / 2);
      break;

    case CONST_INT:
      {
	switch (outer_code)
	  {
	  case HIGH:
	  case LO_SUM:
	    *total = COSTS_N_INSNS (0);
	    return true;

	  case AND:
	  case XOR:
	  case IOR:
	  case ASHIFT:
	  case ASHIFTRT:
	  case LSHIFTRT:
	  case ROTATE:
	  case ROTATERT:
	    if (satisfies_constraint_L (x))
	      *total = COSTS_N_INSNS (0);
	    else
	      *total = COSTS_N_INSNS (2);
	    return true;

	  case SET:
	  case PLUS:
	  case MINUS:
	  case COMPARE:
	    if (satisfies_constraint_K (x))
	      *total = COSTS_N_INSNS (0);
	    else
	      *total = COSTS_N_INSNS (2);
	    return true;

	  case MULT:
	    if (TARGET_MULTIPLY_ENABLED)
	      {
	        if (satisfies_constraint_K (x))
	         *total = COSTS_N_INSNS (0);
	        else
	          *total = COSTS_N_INSNS (2);
		return true;
	      }
	    /* Fall through.  */

	  default:
            if (satisfies_constraint_K (x))
	      *total = COSTS_N_INSNS (1);
	    else
	      *total = COSTS_N_INSNS (2);
	    return true;
	  }
      }

    case SYMBOL_REF:
    case CONST:
      switch (outer_code)
	{
	case HIGH:
	case LO_SUM:
	  *total = COSTS_N_INSNS (0);
	  return true;

	case MEM:
	case SET:
	  if (g_switch_value)
	    {
	      *total = COSTS_N_INSNS (0);
	      return true;
	    }
	  break;
	}
      /* Fall through.  */

    case LABEL_REF:
    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (2);
      return true;

    case SET:
      *total = COSTS_N_INSNS (1);
      break;

    case MEM:
      if (!speed)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (load_latency);
      break;

    }

  return false;
}

/* Implemenent TARGET_CAN_ELIMINATE.  */

bool
lm32_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == STACK_POINTER_REGNUM && frame_pointer_needed) ? false : true;
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
lm32_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x,
			   bool strict, code_helper)
{
   /* (rM) */
  if (strict && REG_P (x) && STRICT_REG_OK_FOR_BASE_P (x))
    return true;
  if (!strict && REG_P (x) && NONSTRICT_REG_OK_FOR_BASE_P (x))
    return true;

  /* (rM)+literal) */
  if (GET_CODE (x) == PLUS
     && REG_P (XEXP (x, 0))
     && ((strict && STRICT_REG_OK_FOR_BASE_P (XEXP (x, 0)))
         || (!strict && NONSTRICT_REG_OK_FOR_BASE_P (XEXP (x, 0))))
     && GET_CODE (XEXP (x, 1)) == CONST_INT
     && satisfies_constraint_K (XEXP ((x), 1)))
    return true;

  /* gp(sym)  */
  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_SMALL_P (x))
    return true;

  return false;
}

/* Check a move is not memory to memory.  */

bool
lm32_move_ok (machine_mode mode, rtx operands[2]) {
  if (memory_operand (operands[0], mode))
    return register_or_zero_operand (operands[1], mode);
  return true;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
lm32_hard_regno_mode_ok (unsigned int regno, machine_mode)
{
  return G_REG_P (regno);
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
lm32_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  return (GET_MODE_CLASS (mode1) == MODE_INT
	  && GET_MODE_CLASS (mode2) == MODE_INT
	  && GET_MODE_SIZE (mode1) <= UNITS_PER_WORD
	  && GET_MODE_SIZE (mode2) <= UNITS_PER_WORD);
}

/* Implement TARGET_STARTING_FRAME_OFFSET.  */

static HOST_WIDE_INT
lm32_starting_frame_offset (void)
{
  return UNITS_PER_WORD;
}

#include "gt-lm32.h"
