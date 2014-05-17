/* Target Code for moxie
   Copyright (C) 2008-2014 Free Software Foundation, Inc.
   Contributed by Anthony Green.

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
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "reload.h"
#include "diagnostic-core.h"
#include "obstack.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "langhooks.h"
#include "df.h"

#define LOSE_AND_RETURN(msgid, x)		\
  do						\
    {						\
      moxie_operand_lossage (msgid, x);		\
      return;					\
    } while (0)

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
moxie_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 2 * UNITS_PER_WORD);
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its
   FUNCTION_DECL; otherwise, FUNC is 0.  

   We always return values in register $r0 for moxie.  */

static rtx
moxie_function_value (const_tree valtype, 
		      const_tree fntype_or_decl ATTRIBUTE_UNUSED,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), MOXIE_R0);
}

/* Define how to find the value returned by a library function.

   We always return values in register $r0 for moxie.  */

static rtx
moxie_libcall_value (enum machine_mode mode,
                     const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, MOXIE_R0);
}

/* Handle TARGET_FUNCTION_VALUE_REGNO_P.

   We always return values in register $r0 for moxie.  */

static bool
moxie_function_value_regno_p (const unsigned int regno)
{
  return (regno == MOXIE_R0);
}

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */

static void
moxie_operand_lossage (const char *msgid, rtx op)
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */

void
moxie_print_operand_address (FILE *file, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (x)]);
      break;
      
    case PLUS:
      switch (GET_CODE (XEXP (x, 1)))
	{
	case CONST_INT:
	  fprintf (file, "%ld(%s)", 
		   INTVAL(XEXP (x, 1)), reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case SYMBOL_REF:
	  output_addr_const (file, XEXP (x, 1));
	  fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case CONST:
	  {
	    rtx plus = XEXP (XEXP (x, 1), 0);
	    if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF 
		&& CONST_INT_P (XEXP (plus, 1)))
	      {
		output_addr_const(file, XEXP (plus, 0));
		fprintf (file,"+%ld(%s)", INTVAL (XEXP (plus, 1)),
			 reg_names[REGNO (XEXP (x, 0))]);
	      }
	    else
	      abort();
	  }
	  break;
	default:
	  abort();
	}
      break;

    default:
      output_addr_const (file, x);
      break;
    }
}

/* The PRINT_OPERAND worker.  */

void
moxie_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 0:
      /* No code, print as usual.  */
      break;

    default:
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      if (REGNO (operand) > MOXIE_R13)
	internal_error ("internal error: bad register: %d", REGNO (operand));
      fprintf (file, "%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (XEXP (operand, 0));
      return;

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CONSTANT_P (operand))
	{
	  output_addr_const (file, operand);
	  return;
	}

      LOSE_AND_RETURN ("unexpected operand", x);
    }
}

/* Per-function machine data.  */
struct GTY(()) machine_function
 {
   /* Number of bytes saved on the stack for callee saved registers.  */
   int callee_saved_reg_size;

   /* Number of bytes saved on the stack for local variables.  */
   int local_vars_size;

   /* The sum of 2 sizes: locals vars and padding byte for saving the
    * registers.  Used in expand_prologue () and expand_epilogue().  */
   int size_for_adjusting_sp;
 };

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
moxie_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* The TARGET_OPTION_OVERRIDE worker.
   All this curently does is set init_machine_status.  */
static void
moxie_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = moxie_init_machine_status;
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  */

static void
moxie_compute_frame (void)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;
  int regno;

  /* Padding needed for each element of the frame.  */
  cfun->machine->local_vars_size = get_frame_size ();

  /* Align to the stack alignment.  */
  padding_locals = cfun->machine->local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun->machine->local_vars_size += padding_locals;

  cfun->machine->callee_saved_reg_size = 0;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (df_regs_ever_live_p (regno) && (! call_used_regs[regno]))
      cfun->machine->callee_saved_reg_size += 4;

  cfun->machine->size_for_adjusting_sp = 
    crtl->args.pretend_args_size
    + cfun->machine->local_vars_size 
    + (ACCUMULATE_OUTGOING_ARGS ? crtl->outgoing_args_size : 0);
}

void
moxie_expand_prologue (void)
{
  int regno;
  rtx insn;

  moxie_compute_frame ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->size_for_adjusting_sp;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (!fixed_regs[regno] && df_regs_ever_live_p (regno) && !call_used_regs[regno])
	{
	  insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, regno)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  if (cfun->machine->size_for_adjusting_sp > 0)
    {
      int i = cfun->machine->size_for_adjusting_sp; 
      while ((i >= 255) && (i <= 510))
	{
	  insn = emit_insn (gen_subsi3 (stack_pointer_rtx, 
					stack_pointer_rtx, 
					GEN_INT (255)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  i -= 255;
	}
      if (i <= 255)
	{
	  insn = emit_insn (gen_subsi3 (stack_pointer_rtx, 
					stack_pointer_rtx, 
					GEN_INT (i)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else
	{
	  rtx reg = gen_rtx_REG (SImode, MOXIE_R12);
	  insn = emit_move_insn (reg, GEN_INT (i));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  insn = emit_insn (gen_subsi3 (stack_pointer_rtx, 
					stack_pointer_rtx, 
					reg));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

void
moxie_expand_epilogue (void)
{
  int regno;
  rtx reg;

  if (cfun->machine->callee_saved_reg_size != 0)
    {
      reg = gen_rtx_REG (Pmode, MOXIE_R12);
      if (cfun->machine->callee_saved_reg_size <= 255)
	{
	  emit_move_insn (reg, hard_frame_pointer_rtx);
	  emit_insn (gen_subsi3 
		     (reg, reg, 
		      GEN_INT (cfun->machine->callee_saved_reg_size)));
	}
      else
	{
	  emit_move_insn (reg,
			  GEN_INT (-cfun->machine->callee_saved_reg_size));
	  emit_insn (gen_addsi3 (reg, reg, hard_frame_pointer_rtx));
	}
      for (regno = FIRST_PSEUDO_REGISTER; regno-- > 0; )
	if (!fixed_regs[regno] && !call_used_regs[regno]
	    && df_regs_ever_live_p (regno))
	  {
	    rtx preg = gen_rtx_REG (Pmode, regno);
	    emit_insn (gen_movsi_pop (reg, preg));
	  }
    }

  emit_jump_insn (gen_returner ());
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */

int
moxie_initial_elimination_offset (int from, int to)
{
  int ret;
  
  if ((from) == FRAME_POINTER_REGNUM && (to) == HARD_FRAME_POINTER_REGNUM)
    {
      /* Compute this since we need to use cfun->machine->local_vars_size.  */
      moxie_compute_frame ();
      ret = -cfun->machine->callee_saved_reg_size;
    }
  else if ((from) == ARG_POINTER_REGNUM && (to) == HARD_FRAME_POINTER_REGNUM)
    ret = 0x00;
  else
    abort ();

  return ret;
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
moxie_setup_incoming_varargs (cumulative_args_t cum_v,
			      enum machine_mode mode ATTRIBUTE_UNUSED,
			      tree type ATTRIBUTE_UNUSED,
			      int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int regno;
  int regs = 8 - *cum;
  
  *pretend_size = regs < 0 ? 0 : GET_MODE_SIZE (SImode) * regs;
  
  if (no_rtl)
    return;
  
  for (regno = *cum; regno < 8; regno++)
    {
      rtx reg = gen_rtx_REG (SImode, regno);
      rtx slot = gen_rtx_PLUS (Pmode,
			       gen_rtx_REG (SImode, ARG_POINTER_REGNUM),
			       GEN_INT (UNITS_PER_WORD * (3 + (regno-2))));
      
      emit_move_insn (gen_rtx_MEM (SImode, slot), reg);
    }
}


/* Return the fixed registers used for condition codes.  */

static bool
moxie_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REG;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Return the next register to be used to hold a function argument or
   NULL_RTX if there's no more space.  */

static rtx
moxie_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		    const_tree type ATTRIBUTE_UNUSED,
		    bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (*cum < 8)
    return gen_rtx_REG (mode, *cum);
  else 
    return NULL_RTX;
}

#define MOXIE_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

static void
moxie_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
			    const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum = (*cum < MOXIE_R6
	  ? *cum + ((3 + MOXIE_FUNCTION_ARG_SIZE (mode, type)) / 4)
	  : *cum);
}

/* Return non-zero if the function argument described by TYPE is to be
   passed by reference.  */

static bool
moxie_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			 enum machine_mode mode, const_tree type,
			 bool named ATTRIBUTE_UNUSED)
{
  unsigned HOST_WIDE_INT size;

  if (type)
    {
      if (AGGREGATE_TYPE_P (type))
	return true;
      size = int_size_in_bytes (type);
    }
  else
    size = GET_MODE_SIZE (mode);

  return size > 4*6;
}

/* Some function arguments will only partially fit in the registers
   that hold arguments.  Given a new arg, return the number of bytes
   that fit in argument passing registers.  */

static int
moxie_arg_partial_bytes (cumulative_args_t cum_v,
			 enum machine_mode mode,
			 tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes_left, size;

  if (*cum >= 8)
    return 0;

  if (moxie_pass_by_reference (cum_v, mode, type, named))
    size = 4;
  else if (type)
    {
      if (AGGREGATE_TYPE_P (type))
	return 0;
      size = int_size_in_bytes (type);
    }
  else
    size = GET_MODE_SIZE (mode);

  bytes_left = (4 * 6) - ((*cum - 2) * 4);

  if (size > bytes_left)
    return bytes_left;
  else
    return 0;
}

/* Worker function for TARGET_STATIC_CHAIN.  */

static rtx
moxie_static_chain (const_tree fndecl, bool incoming_p)
{
  rtx addr, mem;

  if (!DECL_STATIC_CHAIN (fndecl))
    return NULL;

  if (incoming_p)
    addr = plus_constant (Pmode, arg_pointer_rtx, 2 * UNITS_PER_WORD);
  else
    addr = plus_constant (Pmode, stack_pointer_rtx, -UNITS_PER_WORD);

  mem = gen_rtx_MEM (Pmode, addr);
  MEM_NOTRAP_P (mem) = 1;

  return mem;
}

/* Worker function for TARGET_ASM_TRAMPOLINE_TEMPLATE.  */

static void
moxie_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\tpush  $sp, $r0\n");
  fprintf (f, "\tldi.l $r0, 0x0\n");
  fprintf (f, "\tsto.l 0x8($fp), $r0\n");
  fprintf (f, "\tpop   $sp, $r0\n");
  fprintf (f, "\tnop\n");
  fprintf (f, "\tjmpa  0x0\n");
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
moxie_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx mem, fnaddr = XEXP (DECL_RTL (fndecl), 0);

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, 4);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 20);
  emit_move_insn (mem, fnaddr);
}

/* The Global `targetm' Variable.  */

/* Initialize the GCC target structure.  */

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES	hook_bool_const_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY		moxie_return_in_memory
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK	must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        moxie_pass_by_reference
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES        moxie_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG		moxie_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE	moxie_function_arg_advance


#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS 	moxie_setup_incoming_varargs

#undef	TARGET_FIXED_CONDITION_CODE_REGS
#define	TARGET_FIXED_CONDITION_CODE_REGS moxie_fixed_condition_code_regs

/* Define this to return an RTX representing the place where a
   function returns or receives a value of data type RET_TYPE, a tree
   node node representing a data type.  */
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE moxie_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE moxie_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P moxie_function_value_regno_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hook_bool_void_true

#undef TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN moxie_static_chain
#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE moxie_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT moxie_trampoline_init

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE moxie_option_override

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-moxie.h"
