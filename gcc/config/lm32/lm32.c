/* Subroutines used for code generation on the Lattice Mico32 architecture.
   Contributed by Jon Beniston <jon@beniston.com>

   Copyright (C) 2009-2017 Free Software Foundation, Inc.

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
					 machine_mode mode, tree type,
					 int *pretend_size, int no_rtl);
static bool lm32_rtx_costs (rtx x, machine_mode mode, int outer_code, int opno,
			    int *total, bool speed);
static bool lm32_can_eliminate (const int, const int);
static bool
lm32_legitimate_address_p (machine_mode mode, rtx x, bool strict);
static HOST_WIDE_INT lm32_compute_frame_size (int size);
static void lm32_option_override (void);
static rtx lm32_function_arg (cumulative_args_t cum,
			      machine_mode mode, const_tree type,
			      bool named);
static void lm32_function_arg_advance (cumulative_args_t cum,
				       machine_mode mode,
				       const_tree type, bool named);
static bool lm32_hard_regno_mode_ok (unsigned int, machine_mode);
static bool lm32_modes_tieable_p (machine_mode, machine_mode);

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
      if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
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

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

static rtx
lm32_function_arg (cumulative_args_t cum_v, machine_mode mode,
		   const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (mode == VOIDmode)
    /* Compute operand 2 of the call insn.  */
    return GEN_INT (0);

  if (targetm.calls.must_pass_in_stack (mode, type))
    return NULL_RTX;

  if (!named || (*cum + LM32_NUM_REGS2 (mode, type) > LM32_NUM_ARG_REGS))
    return NULL_RTX;

  return gen_rtx_REG (mode, *cum + LM32_FIRST_ARG_REG);
}

static void
lm32_function_arg_advance (cumulative_args_t cum, machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  *get_cumulative_args (cum) += LM32_NUM_REGS2 (mode, type);
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
lm32_setup_incoming_varargs (cumulative_args_t cum_v, machine_mode mode,
			     tree type, int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int first_anon_arg;
  tree fntype;

  fntype = TREE_TYPE (current_function_decl);

  if (stdarg_p (fntype))
    first_anon_arg = *cum + LM32_FIRST_ARG_REG;
  else
    {
      /* this is the common case, we have been passed details setup
         for the last named argument, we want to skip over the
         registers, if any used in passing this named paramter in
         order to determine which is the first registers used to pass
         anonymous arguments.  */
      int size;

      if (mode == BLKmode)
	size = int_size_in_bytes (type);
      else
	size = GET_MODE_SIZE (mode);

      first_anon_arg =
	*cum + LM32_FIRST_ARG_REG +
	((size + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
    }

  if ((first_anon_arg < (LM32_FIRST_ARG_REG + LM32_NUM_ARG_REGS)) && !no_rtl)
    {
      int first_reg_offset = first_anon_arg;
      int size = LM32_FIRST_ARG_REG + LM32_NUM_ARG_REGS - first_anon_arg;
      rtx regblock;

      regblock = gen_rtx_MEM (BLKmode,
			      plus_constant (Pmode, arg_pointer_rtx,
					     FIRST_PARM_OFFSET (0)));
      move_block_from_reg (first_reg_offset, regblock, size);

      *pretend_size = size * UNITS_PER_WORD;
    }
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

  if (TREE_CODE (exp) == VAR_DECL && DECL_SECTION_NAME (exp))
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
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), 0);
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
lm32_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x, bool strict)
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
