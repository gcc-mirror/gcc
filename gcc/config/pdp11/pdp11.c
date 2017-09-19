/* Subroutines for gcc2 for pdp11.
   Copyright (C) 1994-2017 Free Software Foundation, Inc.
   Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

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
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "conditions.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"
#include "dbxout.h"

/* This file should be included last.  */
#include "target-def.h"

/* this is the current value returned by the macro FIRST_PARM_OFFSET 
   defined in tm.h */
int current_first_parm_offset;

/* Routines to encode/decode pdp11 floats */
static void encode_pdp11_f (const struct real_format *fmt,
			    long *, const REAL_VALUE_TYPE *);
static void decode_pdp11_f (const struct real_format *,
			    REAL_VALUE_TYPE *, const long *);
static void encode_pdp11_d (const struct real_format *fmt,
			    long *, const REAL_VALUE_TYPE *);
static void decode_pdp11_d (const struct real_format *,
			    REAL_VALUE_TYPE *, const long *);

/* These two are taken from the corresponding vax descriptors
   in real.c, changing only the encode/decode routine pointers.  */
const struct real_format pdp11_f_format =
  {
    encode_pdp11_f,
    decode_pdp11_f,
    2,
    24,
    24,
    -127,
    127,
    15,
    15,
    0,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    "pdp11_f"
  };

const struct real_format pdp11_d_format =
  {
    encode_pdp11_d,
    decode_pdp11_d,
    2,
    56,
    56,
    -127,
    127,
    15,
    15,
    0,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    "pdp11_d"
  };

static void
encode_pdp11_f (const struct real_format *fmt ATTRIBUTE_UNUSED, long *buf,
		const REAL_VALUE_TYPE *r)
{
  (*vax_f_format.encode) (fmt, buf, r);
  buf[0] = ((buf[0] >> 16) & 0xffff) | ((buf[0] & 0xffff) << 16);
}

static void
decode_pdp11_f (const struct real_format *fmt ATTRIBUTE_UNUSED,
		REAL_VALUE_TYPE *r, const long *buf)
{
  long tbuf;
  tbuf = ((buf[0] >> 16) & 0xffff) | ((buf[0] & 0xffff) << 16);
  (*vax_f_format.decode) (fmt, r, &tbuf);
}

static void
encode_pdp11_d (const struct real_format *fmt ATTRIBUTE_UNUSED, long *buf,
		const REAL_VALUE_TYPE *r)
{
  (*vax_d_format.encode) (fmt, buf, r);
  buf[0] = ((buf[0] >> 16) & 0xffff) | ((buf[0] & 0xffff) << 16);
  buf[1] = ((buf[1] >> 16) & 0xffff) | ((buf[1] & 0xffff) << 16);
}

static void
decode_pdp11_d (const struct real_format *fmt ATTRIBUTE_UNUSED,
		REAL_VALUE_TYPE *r, const long *buf)
{
  long tbuf[2];
  tbuf[0] = ((buf[0] >> 16) & 0xffff) | ((buf[0] & 0xffff) << 16);
  tbuf[1] = ((buf[1] >> 16) & 0xffff) | ((buf[1] & 0xffff) << 16);
  (*vax_d_format.decode) (fmt, r, tbuf);
}

/* This is where the condition code register lives.  */
/* rtx cc0_reg_rtx; - no longer needed? */

static const char *singlemove_string (rtx *);
static bool pdp11_assemble_integer (rtx, unsigned int, int);
static bool pdp11_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static bool pdp11_return_in_memory (const_tree, const_tree);
static rtx pdp11_function_value (const_tree, const_tree, bool);
static rtx pdp11_libcall_value (machine_mode, const_rtx);
static bool pdp11_function_value_regno_p (const unsigned int);
static void pdp11_trampoline_init (rtx, tree, rtx);
static rtx pdp11_function_arg (cumulative_args_t, machine_mode,
			       const_tree, bool);
static void pdp11_function_arg_advance (cumulative_args_t,
					machine_mode, const_tree, bool);
static void pdp11_conditional_register_usage (void);
static bool pdp11_legitimate_constant_p (machine_mode, rtx);

static bool pdp11_scalar_mode_supported_p (scalar_mode);

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP NULL
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP NULL
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER pdp11_assemble_integer

#undef TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN "["
#undef TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN "]"

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS pdp11_rtx_costs

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG pdp11_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE pdp11_function_arg_advance

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY pdp11_return_in_memory

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE pdp11_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE pdp11_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P pdp11_function_value_regno_p

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT pdp11_trampoline_init

#undef  TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD pdp11_secondary_reload

#undef  TARGET_REGISTER_MOVE_COST 
#define TARGET_REGISTER_MOVE_COST pdp11_register_move_cost

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS pdp11_preferred_reload_class

#undef  TARGET_PREFERRED_OUTPUT_RELOAD_CLASS
#define TARGET_PREFERRED_OUTPUT_RELOAD_CLASS pdp11_preferred_output_reload_class

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P pdp11_legitimate_address_p

#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE pdp11_conditional_register_usage

#undef  TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION pdp11_function_section

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND pdp11_asm_print_operand

#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P pdp11_asm_print_operand_punct_valid_p

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P pdp11_legitimate_constant_p

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P pdp11_scalar_mode_supported_p

#undef  TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS pdp11_hard_regno_nregs
#undef  TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK pdp11_hard_regno_mode_ok

#undef  TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P pdp11_modes_tieable_p

#undef  TARGET_SECONDARY_MEMORY_NEEDED
#define TARGET_SECONDARY_MEMORY_NEEDED pdp11_secondary_memory_needed

#undef  TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS pdp11_can_change_mode_class

/* A helper function to determine if REGNO should be saved in the
   current function's stack frame.  */

static inline bool
pdp11_saved_regno (unsigned regno)
{
  return !call_used_regs[regno] && df_regs_ever_live_p (regno);
}

/* Expand the function prologue.  */

void
pdp11_expand_prologue (void)
{							       
  HOST_WIDE_INT fsize = get_frame_size ();
  unsigned regno;
  rtx x, via_ac = NULL;

  /* If we are outputting code for main, the switch FPU to the
     right mode if TARGET_FPU.  */
  if (MAIN_NAME_P (DECL_NAME (current_function_decl)) && TARGET_FPU)
    {
      emit_insn (gen_setd ());
      emit_insn (gen_seti ());
    }
    
  if (frame_pointer_needed) 					
    {								
      x = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
      x = gen_frame_mem (Pmode, x);
      emit_move_insn (x, hard_frame_pointer_rtx);

      emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
    }								

  /* Make frame.  */
  if (fsize)
    {
      emit_insn (gen_addhi3 (stack_pointer_rtx, stack_pointer_rtx,
			     GEN_INT (-fsize)));

      /* Prevent frame references via the frame pointer from being
	 scheduled before the frame is allocated.  */
      if (frame_pointer_needed)
	emit_insn (gen_blockage ());
    }

  /* Save CPU registers.  */
  for (regno = R0_REGNUM; regno <= PC_REGNUM; regno++)
    if (pdp11_saved_regno (regno)
	&& (regno != HARD_FRAME_POINTER_REGNUM || !frame_pointer_needed))
      {
	x = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
	x = gen_frame_mem (Pmode, x);
	emit_move_insn (x, gen_rtx_REG (Pmode, regno));
      }

  /* Save FPU registers.  */
  for (regno = AC0_REGNUM; regno <= AC3_REGNUM; regno++) 
    if (pdp11_saved_regno (regno))
      {
	x = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
	x = gen_frame_mem (DFmode, x);
	via_ac = gen_rtx_REG (DFmode, regno);
	emit_move_insn (x, via_ac);
      }

  /* ??? Maybe make ac4, ac5 call used regs?? */
  for (regno = AC4_REGNUM; regno <= AC5_REGNUM; regno++)
    if (pdp11_saved_regno (regno))
      {
	gcc_assert (via_ac != NULL);
	emit_move_insn (via_ac, gen_rtx_REG (DFmode, regno));

	x = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
	x = gen_frame_mem (DFmode, x);
	emit_move_insn (x, via_ac);
      }
}

/* The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

/* Maybe we can make leaf functions faster by switching to the
   second register file - this way we don't have to save regs!
   leaf functions are ~ 50% of all functions (dynamically!) 

   set/clear bit 11 (dec. 2048) of status word for switching register files - 
   but how can we do this? the pdp11/45 manual says bit may only 
   be set (p.24), but not cleared!

   switching to kernel is probably more expensive, so we'll leave it 
   like this and not use the second set of registers... 

   maybe as option if you want to generate code for kernel mode? */

void
pdp11_expand_epilogue (void)
{								
  HOST_WIDE_INT fsize = get_frame_size ();
  unsigned regno;
  rtx x, reg, via_ac = NULL;

  if (pdp11_saved_regno (AC4_REGNUM) || pdp11_saved_regno (AC5_REGNUM))
    {
      /* Find a temporary with which to restore AC4/5.  */
      for (regno = AC0_REGNUM; regno <= AC3_REGNUM; regno++)
	if (pdp11_saved_regno (regno))
	  {
	    via_ac = gen_rtx_REG (DFmode, regno);
	    break;
	  }
    }

  /* If possible, restore registers via pops.  */
  if (!frame_pointer_needed || crtl->sp_is_unchanging)
    {
      /* Restore registers via pops.  */

      for (regno = AC5_REGNUM; regno >= AC0_REGNUM; regno--)
	if (pdp11_saved_regno (regno))
	  {
	    x = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
	    x = gen_frame_mem (DFmode, x);
	    reg = gen_rtx_REG (DFmode, regno);

	    if (LOAD_FPU_REG_P (regno))
	      emit_move_insn (reg, x);
	    else
	      {
	        emit_move_insn (via_ac, x);
		emit_move_insn (reg, via_ac);
	      }
	  }

      for (regno = PC_REGNUM; regno >= R0_REGNUM + 2; regno--)
	if (pdp11_saved_regno (regno)
	    && (regno != HARD_FRAME_POINTER_REGNUM || !frame_pointer_needed))
	  {
	    x = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
	    x = gen_frame_mem (Pmode, x);
	    emit_move_insn (gen_rtx_REG (Pmode, regno), x);
	  }
    }
  else
    {
      /* Restore registers via moves.  */
      /* ??? If more than a few registers need to be restored, it's smaller
	 to generate a pointer through which we can emit pops.  Consider
	 that moves cost 2*NREG words and pops cost NREG+3 words.  This
	 means that the crossover is NREG=3.

	 Possible registers to use are:
	  (1) The first call-saved general register.  This register will
		be restored with the last pop.
	  (2) R1, if it's not used as a return register.
	  (3) FP itself.  This option may result in +4 words, since we
		may need two add imm,rn instructions instead of just one.
		This also has the downside that we're not representing
		the unwind info in any way, so during the epilogue the
		debugger may get lost.  */

      HOST_WIDE_INT ofs = -pdp11_sp_frame_offset ();

      for (regno = AC5_REGNUM; regno >= AC0_REGNUM; regno--)
	if (pdp11_saved_regno (regno))
	  {
	    x = plus_constant (Pmode, hard_frame_pointer_rtx, ofs);
	    x = gen_frame_mem (DFmode, x);
	    reg = gen_rtx_REG (DFmode, regno);

	    if (LOAD_FPU_REG_P (regno))
	      emit_move_insn (reg, x);
	    else
	      {
	        emit_move_insn (via_ac, x);
		emit_move_insn (reg, via_ac);
	      }
	    ofs += 8;
	  }

      for (regno = PC_REGNUM; regno >= R0_REGNUM + 2; regno--)
	if (pdp11_saved_regno (regno)
	    && (regno != HARD_FRAME_POINTER_REGNUM || !frame_pointer_needed))
	  {
	    x = plus_constant (Pmode, hard_frame_pointer_rtx, ofs);
	    x = gen_frame_mem (Pmode, x);
	    emit_move_insn (gen_rtx_REG (Pmode, regno), x);
	    ofs += 2;
	  }
    }

  /* Deallocate the stack frame.  */
  if (fsize)
    {
      /* Prevent frame references via any pointer from being
	 scheduled after the frame is deallocated.  */
      emit_insn (gen_blockage ());

      if (frame_pointer_needed)
	{
	  /* We can deallocate the frame with a single move.  */
	  emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
	}
      else
	emit_insn (gen_addhi3 (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (fsize)));
    }

  if (frame_pointer_needed)
    {
      x = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
      x = gen_frame_mem (Pmode, x);
      emit_move_insn (hard_frame_pointer_rtx, x);
    }

  emit_jump_insn (gen_return ());
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */
static const char *
singlemove_string (rtx *operands)
{
  if (operands[1] != const0_rtx)
    return "mov %1,%0";

  return "clr %0";
}


/* Expand multi-word operands (SImode or DImode) into the 2 or 4
   corresponding HImode operands.  The number of operands is given
   as the third argument, and the required order of the parts as
   the fourth argument.  */
bool
pdp11_expand_operands (rtx *operands, rtx exops[][2], int opcount, 
		       pdp11_action *action, pdp11_partorder order)
{
  int words, op, w, i, sh;
  pdp11_partorder useorder;
  bool sameoff = false;
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype;
  long sval[2];
  
  words = GET_MODE_BITSIZE (GET_MODE (operands[0])) / 16;
  
  /* If either piece order is accepted and one is pre-decrement
     while the other is post-increment, set order to be high order
     word first.  That will force the pre-decrement to be turned
     into a pointer adjust, then offset addressing.
     Otherwise, if either operand uses pre-decrement, that means
     the order is low order first. 
     Otherwise, if both operands are registers and destination is
     higher than source and they overlap, do low order word (highest
     register number) first.  */
  useorder = either;
  if (opcount == 2)
    {
      if (!REG_P (operands[0]) && !REG_P (operands[1]) &&
	  !(CONSTANT_P (operands[1]) || 
	    GET_CODE (operands[1]) == CONST_DOUBLE) &&
	  ((GET_CODE (XEXP (operands[0], 0)) == POST_INC &&
	    GET_CODE (XEXP (operands[1], 0)) == PRE_DEC) ||
	   (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC &&
	    GET_CODE (XEXP (operands[1], 0)) == POST_INC)))
	    useorder = big;
      else if ((!REG_P (operands[0]) &&
		GET_CODE (XEXP (operands[0], 0)) == PRE_DEC) ||
	       (!REG_P (operands[1]) &&
		!(CONSTANT_P (operands[1]) || 
		  GET_CODE (operands[1]) == CONST_DOUBLE) &&
		GET_CODE (XEXP (operands[1], 0)) == PRE_DEC))
	useorder = little;
      else if (REG_P (operands[0]) && REG_P (operands[1]) &&
	       REGNO (operands[0]) > REGNO (operands[1]) &&
	       REGNO (operands[0]) < REGNO (operands[1]) + words)
	    useorder = little;

      /* Check for source == offset from register and dest == push of
	 the same register.  In that case, we have to use the same
	 offset (the one for the low order word) for all words, because
	 the push increases the offset to each source word.
	 In theory there are other cases like this, for example dest == pop,
	 but those don't occur in real life so ignore those.  */
      if (GET_CODE (operands[0]) ==  MEM 
	  && GET_CODE (XEXP (operands[0], 0)) == PRE_DEC
	  && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
	  && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
	sameoff = true;
    }

  /* If the caller didn't specify order, use the one we computed,
     or high word first if we don't care either.  If the caller did
     specify, verify we don't have a problem with that order.
     (If it matters to the caller, constraints need to be used to
     ensure this case doesn't occur).  */
  if (order == either)
    order = (useorder == either) ? big : useorder;
  else
    gcc_assert (useorder == either || useorder == order);

  
  for (op = 0; op < opcount; op++)
    {
      /* First classify the operand.  */
      if (REG_P (operands[op]))
	optype = REGOP;
      else if (CONSTANT_P (operands[op])
	       || GET_CODE (operands[op]) == CONST_DOUBLE)
	optype = CNSTOP;
      else if (GET_CODE (XEXP (operands[op], 0)) == POST_INC)
	optype = POPOP;
      else if (GET_CODE (XEXP (operands[op], 0)) == PRE_DEC)
	optype = PUSHOP;
      else if (!reload_in_progress || offsettable_memref_p (operands[op]))
	optype = OFFSOP;
      else if (GET_CODE (operands[op]) == MEM)
	optype = MEMOP;
      else
	optype = RNDOP;

      /* Check for the cases that the operand constraints are not
	 supposed to allow to happen. Return failure for such cases.  */
      if (optype == RNDOP)
	return false;
      
      if (action != NULL)
	action[op] = no_action;
      
      /* If the operand uses pre-decrement addressing but we
	 want to get the parts high order first,
	 decrement the former register explicitly
	 and change the operand into ordinary indexing.  */
      if (optype == PUSHOP && order == big)
	{
	  gcc_assert (action != NULL);
	  action[op] = dec_before;
	  operands[op] = gen_rtx_MEM (GET_MODE (operands[op]),
				      XEXP (XEXP (operands[op], 0), 0));
	  optype = OFFSOP;
	}
      /* If the operand uses post-increment mode but we want 
	 to get the parts low order first, change the operand
	 into ordinary indexing and remember to increment
	 the register explicitly when we're done.  */
      else if (optype == POPOP && order == little)
	{
	  gcc_assert (action != NULL);
	  action[op] = inc_after;
	  operands[op] = gen_rtx_MEM (GET_MODE (operands[op]),
				      XEXP (XEXP (operands[op], 0), 0));
	  optype = OFFSOP;
	}

      if (GET_CODE (operands[op]) == CONST_DOUBLE)
	REAL_VALUE_TO_TARGET_DOUBLE
	  (*CONST_DOUBLE_REAL_VALUE (operands[op]), sval);
      
      for (i = 0; i < words; i++)
	{
	  if (order == big)
	    w = i;
	  else if (sameoff)
	    w = words - 1;
	  else
	    w = words - 1 - i;

	  /* Set the output operand to be word "w" of the input.  */
	  if (optype == REGOP)
	    exops[i][op] = gen_rtx_REG (HImode, REGNO (operands[op]) + w);
	  else if (optype == OFFSOP)
	    exops[i][op] = adjust_address (operands[op], HImode, w * 2);
	  else if (optype == CNSTOP)
	    {
	      if (GET_CODE (operands[op]) == CONST_DOUBLE)
		{
		  sh = 16 - (w & 1) * 16;
		  exops[i][op] = gen_rtx_CONST_INT (HImode, (sval[w / 2] >> sh) & 0xffff);
		}
	      else
		{
		  sh = ((words - 1 - w) * 16);
		  exops[i][op] = gen_rtx_CONST_INT (HImode, trunc_int_for_mode (INTVAL(operands[op]) >> sh, HImode));
		}
	    }
	  else
	    exops[i][op] = operands[op];
	}
    }
  return true;
}

/* Output assembler code to perform a multiple-word move insn
   with operands OPERANDS.  This moves 2 or 4 words depending
   on the machine mode of the operands.  */

const char *
output_move_multiple (rtx *operands)
{
  rtx exops[4][2];
  pdp11_action action[2];
  int i, words;
  
  words = GET_MODE_BITSIZE (GET_MODE (operands[0])) / 16;

  pdp11_expand_operands (operands, exops, 2, action, either);
  
  /* Check for explicit decrement before.  */
  if (action[0] == dec_before)
    {
      operands[0] = XEXP (operands[0], 0);
      output_asm_insn ("sub $4,%0", operands);
    }
  if (action[1] == dec_before)
    {
      operands[1] = XEXP (operands[1], 0);
      output_asm_insn ("sub $4,%1", operands);
    }

  /* Do the words.  */
  for (i = 0; i < words; i++)
    output_asm_insn (singlemove_string (exops[i]), exops[i]);

  /* Check for increment after.  */
  if (action[0] == inc_after)
    {
      operands[0] = XEXP (operands[0], 0);
      output_asm_insn ("add $4,%0", operands);
    }
  if (action[1] == inc_after)
    {
      operands[1] = XEXP (operands[1], 0);
      output_asm_insn ("add $4,%1", operands);
    }

  return "";
}

/* Output an ascii string.  */
void
output_ascii (FILE *file, const char *p, int size)
{
  int i;

  /* This used to output .byte "string", which doesn't work with the UNIX
     assembler and I think not with DEC ones either.  */
  fprintf (file, "\t.byte ");

  for (i = 0; i < size; i++)
    {
      register int c = p[i];
      if (c < 0)
	c += 256;
      fprintf (file, "%#o", c);
      if (i < size - 1)
	putc (',', file);
    }
  putc ('\n', file);
}


void
pdp11_asm_output_var (FILE *file, const char *name, int size,
		      int align, bool global)
{
  if (align > 8)
    fprintf (file, "\n\t.even\n");
  if (global)
    {
      fprintf (file, ".globl ");
      assemble_name (file, name);
    }
  fprintf (file, "\n");
  assemble_name (file, name);
  fprintf (file, ": .=.+ %#ho\n", (unsigned short)size);
}

static void
pdp11_asm_print_operand (FILE *file, rtx x, int code)
{
  long sval[2];
 
  if (code == '#')
    fprintf (file, "#");
  else if (code == '@')
    {
      if (TARGET_UNIX_ASM)
	fprintf (file, "*");
      else
	fprintf (file, "@");
    }
  else if (GET_CODE (x) == REG)
    fprintf (file, "%s", reg_names[REGNO (x)]);
  else if (GET_CODE (x) == MEM)
    output_address (GET_MODE (x), XEXP (x, 0));
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != SImode)
    {
      REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (x), sval);
      fprintf (file, "$%#lo", sval[0] >> 16);
    }
  else
    {
      putc ('$', file);
      output_addr_const_pdp11 (file, x);
    }
}

static bool
pdp11_asm_print_operand_punct_valid_p (unsigned char c)
{
  return (c == '#' || c == '@');
}

void
print_operand_address (FILE *file, register rtx addr)
{
  register rtx breg;
  rtx offset;
  int again = 0;
  
 retry:

  switch (GET_CODE (addr))
    {
    case MEM:
      if (TARGET_UNIX_ASM)
	fprintf (file, "*");
      else
	fprintf (file, "@");
      addr = XEXP (addr, 0);
      again = 1;
      goto retry;

    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (addr)]);
      break;

    case PRE_MODIFY:
    case PRE_DEC:
      fprintf (file, "-(%s)", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_MODIFY:
    case POST_INC:
      fprintf (file, "(%s)+", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:
      breg = 0;
      offset = 0;
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0))
	  || GET_CODE (XEXP (addr, 0)) == MEM)
	{
	  offset = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1))
	       || GET_CODE (XEXP (addr, 1)) == MEM)
	{
	  offset = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) != PLUS)
	;
      else if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  breg = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	{
	  breg = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) == REG)
	{
	  gcc_assert (breg == 0);
	  breg = addr;
	  addr = 0;
	}
      if (offset != 0)
	{
	  gcc_assert (addr == 0);
	  addr = offset;
	}
      if (addr != 0)
	output_addr_const_pdp11 (file, addr);
      if (breg != 0)
	{
	  gcc_assert (GET_CODE (breg) == REG);
	  fprintf (file, "(%s)", reg_names[REGNO (breg)]);
	}
      break;

    default:
      if (!again && GET_CODE (addr) == CONST_INT)
	{
	  /* Absolute (integer number) address.  */
	  if (!TARGET_UNIX_ASM)
	    fprintf (file, "@$");
	}
      output_addr_const_pdp11 (file, addr);
    }
}

/* Target hook to assemble integer objects.  We need to use the
   pdp-specific version of output_addr_const.  */

static bool
pdp11_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (aligned_p)
    switch (size)
      {
      case 1:
	fprintf (asm_out_file, "\t.byte\t");
	output_addr_const_pdp11 (asm_out_file, GEN_INT (INTVAL (x) & 0xff));
;
	fprintf (asm_out_file, " /* char */\n");
	return true;

      case 2:
	fprintf (asm_out_file, TARGET_UNIX_ASM ? "\t" : "\t.word\t");
	output_addr_const_pdp11 (asm_out_file, x);
	fprintf (asm_out_file, " /* short */\n");
	return true;
      }
  return default_assemble_integer (x, size, aligned_p);
}


/* register move costs, indexed by regs */

static const int move_costs[N_REG_CLASSES][N_REG_CLASSES] = 
{
             /* NO  MUL  GEN  LFPU  NLFPU FPU ALL */

/* NO */     {  0,   0,   0,    0,    0,    0,   0},
/* MUL */    {  0,   2,   2,   22,   22,   22,  22},
/* GEN */    {  0,   2,   2,   22,   22,   22,  22},
/* LFPU */   {  0,  22,  22,    2,    2,    2,  22},
/* NLFPU */  {  0,  22,  22,    2,   10,   10,  22},
/* FPU */    {  0,  22,  22,    2,   10,   10,  22},
/* ALL */    {  0,  22,  22,   22,   22,   22,  22}
}  ;


/* -- note that some moves are tremendously expensive, 
   because they require lots of tricks! do we have to 
   charge the costs incurred by secondary reload class 
   -- as we do here with 10 -- or not ? */

static int 
pdp11_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			  reg_class_t c1, reg_class_t c2)
{
    return move_costs[(int)c1][(int)c2];
}

static bool
pdp11_rtx_costs (rtx x, machine_mode mode, int outer_code ATTRIBUTE_UNUSED,
		 int opno ATTRIBUTE_UNUSED, int *total,
		 bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      if (INTVAL (x) == 0 || INTVAL (x) == -1 || INTVAL (x) == 1)
	{
	  *total = 0;
	  return true;
	}
      /* FALLTHRU */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      /* Twice as expensive as REG.  */
      *total = 2;
      return true;

    case CONST_DOUBLE:
      /* Twice (or 4 times) as expensive as 16 bit.  */
      *total = 4;
      return true;

    case MULT:
      /* ??? There is something wrong in MULT because MULT is not 
         as cheap as total = 2 even if we can shift!  */
      /* If optimizing for size make mult etc cheap, but not 1, so when 
         in doubt the faster insn is chosen.  */
      if (optimize_size)
        *total = COSTS_N_INSNS (2);
      else
        *total = COSTS_N_INSNS (11);
      return false;

    case DIV:
      if (optimize_size)
        *total = COSTS_N_INSNS (2);
      else
        *total = COSTS_N_INSNS (25);
      return false;

    case MOD:
      if (optimize_size)
        *total = COSTS_N_INSNS (2);
      else
        *total = COSTS_N_INSNS (26);
      return false;

    case ABS:
      /* Equivalent to length, so same for optimize_size.  */
      *total = COSTS_N_INSNS (3);
      return false;

    case ZERO_EXTEND:
      /* Only used for qi->hi.  */
      *total = COSTS_N_INSNS (1);
      return false;

    case SIGN_EXTEND:
      if (mode == HImode)
      	*total = COSTS_N_INSNS (1);
      else if (mode == SImode)
	*total = COSTS_N_INSNS (6);
      else
	*total = COSTS_N_INSNS (2);
      return false;

    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
      if (optimize_size)
        *total = COSTS_N_INSNS (1);
      else if (mode ==  QImode)
        {
          if (GET_CODE (XEXP (x, 1)) != CONST_INT)
   	    *total = COSTS_N_INSNS (8); /* worst case */
          else
	    *total = COSTS_N_INSNS (INTVAL (XEXP (x, 1)));
        }
      else if (mode == HImode)
        {
          if (GET_CODE (XEXP (x, 1)) == CONST_INT)
            {
	      if (abs (INTVAL (XEXP (x, 1))) == 1)
                *total = COSTS_N_INSNS (1);
              else
	        *total = COSTS_N_INSNS (2.5 + 0.5 * INTVAL (XEXP (x, 1)));
            }
          else
            *total = COSTS_N_INSNS (10); /* worst case */
        }
      else if (mode == SImode)
        {
          if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    *total = COSTS_N_INSNS (2.5 + 0.5 * INTVAL (XEXP (x, 1)));
          else /* worst case */
            *total = COSTS_N_INSNS (18);
        }
      return false;

    default:
      return false;
    }
}

const char *
output_jump (enum rtx_code code, int inv, int length)
{
    static int x = 0;
    
    static char buf[1000];
    const char *pos, *neg;

    if (cc_prev_status.flags & CC_NO_OVERFLOW)
      {
	switch (code)
	  {
	  case GTU: code = GT; break;
	  case LTU: code = LT; break;
	  case GEU: code = GE; break;
	  case LEU: code = LE; break;
	  default: ;
	  }
      }
    switch (code)
      {
      case EQ: pos = "beq", neg = "bne"; break;
      case NE: pos = "bne", neg = "beq"; break;
      case GT: pos = "bgt", neg = "ble"; break;
      case GTU: pos = "bhi", neg = "blos"; break;
      case LT: pos = "blt", neg = "bge"; break;
      case LTU: pos = "blo", neg = "bhis"; break;
      case GE: pos = "bge", neg = "blt"; break;
      case GEU: pos = "bhis", neg = "blo"; break;
      case LE: pos = "ble", neg = "bgt"; break;
      case LEU: pos = "blos", neg = "bhi"; break;
      default: gcc_unreachable ();
      }

#if 0
/* currently we don't need this, because the tstdf and cmpdf 
   copy the condition code immediately, and other float operations are not 
   yet recognized as changing the FCC - if so, then the length-cost of all
   jump insns increases by one, because we have to potentially copy the 
   FCC! */
    if (cc_status.flags & CC_IN_FPU)
	output_asm_insn("cfcc", NULL);
#endif
	
    switch (length)
    {
      case 2:
	
	sprintf(buf, "%s %%l1", inv ? neg : pos);
	
	return buf;
	
      case 6:
	
	sprintf(buf, "%s JMP_%d\n\tjmp %%l1\nJMP_%d:", inv ? pos : neg, x, x);
	
	x++;
	
	return buf;
	
      default:
	
	gcc_unreachable ();
    }
    
}

void
notice_update_cc_on_set(rtx exp, rtx insn ATTRIBUTE_UNUSED)
{
    if (GET_CODE (SET_DEST (exp)) == CC0)
    { 
      cc_status.flags = 0;					
      cc_status.value1 = SET_DEST (exp);			
      cc_status.value2 = SET_SRC (exp);			
    }							
    else if (GET_CODE (SET_SRC (exp)) == CALL)		
    { 
      CC_STATUS_INIT; 
    }
    else if (SET_DEST(exp) == pc_rtx)
    { 
      /* jump */
    }	
    else if (GET_MODE (SET_DEST(exp)) == HImode		
	     || GET_MODE (SET_DEST(exp)) == QImode)
    { 
      cc_status.flags = GET_CODE (SET_SRC(exp)) == MINUS ? 0 : CC_NO_OVERFLOW;
      cc_status.value1 = SET_SRC (exp);   			
      cc_status.value2 = SET_DEST (exp);			
	
      if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	
	  && cc_status.value2					
	  && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))
	cc_status.value2 = 0;					
      if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM	
	  && cc_status.value2					
	  && GET_CODE (cc_status.value2) == MEM)			
	cc_status.value2 = 0; 					
    }		        
    else
    { 
      CC_STATUS_INIT; 
    }
}


int
simple_memory_operand(rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
    rtx addr;

    /* Eliminate non-memory operations */
    if (GET_CODE (op) != MEM)
	return FALSE;

#if 0
    /* dword operations really put out 2 instructions, so eliminate them.  */
    if (GET_MODE_SIZE (GET_MODE (op)) > (HAVE_64BIT_P () ? 8 : 4))
	return FALSE;
#endif

    /* Decode the address now.  */

  indirection:
    
    addr = XEXP (op, 0);

    switch (GET_CODE (addr))
    {
      case REG:
	/* (R0) - no extra cost */
	return 1;
	
      case PRE_DEC:
      case POST_INC:
	/* -(R0), (R0)+ - cheap! */
	return 0;
	
      case MEM:
	/* cheap - is encoded in addressing mode info! 

	   -- except for @(R0), which has to be @0(R0) !!! */

	if (GET_CODE (XEXP (addr, 0)) == REG)
	    return 0;
	
	op=addr;
	goto indirection;
	
      case CONST_INT:
      case LABEL_REF:	       
      case CONST:
      case SYMBOL_REF:
	/* @#address - extra cost */
	return 0;

      case PLUS:
	/* X(R0) - extra cost */
	return 0;

      default:
	break;
    }
    
    return FALSE;
}


/*
 * output a block move:
 *
 * operands[0]	... to
 * operands[1]  ... from
 * operands[2]  ... length
 * operands[3]  ... alignment
 * operands[4]  ... scratch register
 */

 
const char *
output_block_move(rtx *operands)
{
    static int count = 0;
    char buf[200];
    int unroll;
    int lastbyte = 0;
    
    /* Move of zero bytes is a NOP.  */
    if (operands[2] == const0_rtx)
      return "";
    
    /* Look for moves by small constant byte counts, those we'll
       expand to straight line code.  */
    if (CONSTANT_P (operands[2]))
    {
	if (INTVAL (operands[2]) < 16
	    && (!optimize_size || INTVAL (operands[2]) < 5)
	    && INTVAL (operands[3]) == 1)
	{
	    register int i;
	    
	    for (i = 1; i <= INTVAL (operands[2]); i++)
		output_asm_insn("movb (%1)+, (%0)+", operands);

	    return "";
	}
	else if (INTVAL(operands[2]) < 32
		 && (!optimize_size || INTVAL (operands[2]) < 9)
		 && INTVAL (operands[3]) >= 2)
	{
	    register int i;
	    
	    for (i = 1; i <= INTVAL (operands[2]) / 2; i++)
		output_asm_insn ("mov (%1)+, (%0)+", operands);
	    if (INTVAL (operands[2]) & 1)
	      output_asm_insn ("movb (%1), (%0)", operands);
	    
	    return "";
	}
    }

    /* Ideally we'd look for moves that are multiples of 4 or 8
       bytes and handle those by unrolling the move loop.  That
       makes for a lot of code if done at run time, but it's ok
       for constant counts.  Also, for variable counts we have
       to worry about odd byte count with even aligned pointers.
       On 11/40 and up we handle that case; on older machines
       we don't and just use byte-wise moves all the time.  */

    if (CONSTANT_P (operands[2]) )
    {
      if (INTVAL (operands[3]) < 2)
	unroll = 0;
      else
	{
	  lastbyte = INTVAL (operands[2]) & 1;

	  if (optimize_size || INTVAL (operands[2]) & 2)
	    unroll = 1;
	  else if (INTVAL (operands[2]) & 4)
	    unroll = 2;
	  else
	    unroll = 3;
	}
      
      /* Loop count is byte count scaled by unroll.  */
      operands[2] = GEN_INT (INTVAL (operands[2]) >> unroll);
      output_asm_insn ("mov %2, %4", operands);
    }
    else
    {
	/* Variable byte count; use the input register
	   as the scratch.  */
	operands[4] = operands[2];

	/* Decide whether to move by words, and check
	   the byte count for zero.  */
	if (TARGET_40_PLUS && INTVAL (operands[3]) > 1)
	  {
	    unroll = 1;
	    output_asm_insn ("asr %4", operands);
	  }
	else
	  {
	    unroll = 0;
	    output_asm_insn ("tst %4", operands);
	  }
	sprintf (buf, "beq movestrhi%d", count + 1);
	output_asm_insn (buf, NULL);
    }

    /* Output the loop label.  */
    sprintf (buf, "\nmovestrhi%d:", count);
    output_asm_insn (buf, NULL);

    /* Output the appropriate move instructions.  */
    switch (unroll)
    {
      case 0:
	output_asm_insn ("movb (%1)+, (%0)+", operands);
	break;
	
      case 1:
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	break;
	
      case 2:
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	break;
	
      default:
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	output_asm_insn ("mov (%1)+, (%0)+", operands);
	break;
    }

    /* Output the decrement and test.  */
    if (TARGET_40_PLUS)
      {
	sprintf (buf, "sob %%4, movestrhi%d", count);
	output_asm_insn (buf, operands);
      }
    else
      {
	output_asm_insn ("dec %4", operands);
	sprintf (buf, "bgt movestrhi%d", count);
	output_asm_insn (buf, NULL);
      }
    count ++;

    /* If constant odd byte count, move the last byte.  */
    if (lastbyte)
      output_asm_insn ("movb (%1), (%0)", operands);
    else if (!CONSTANT_P (operands[2]))
      {
	/* Output the destination label for the zero byte count check.  */
	sprintf (buf, "\nmovestrhi%d:", count);
	output_asm_insn (buf, NULL);
	count++;
    
	/* If we did word moves, check for trailing last byte. */
	if (unroll)
	  {
	    sprintf (buf, "bcc movestrhi%d", count);
	    output_asm_insn (buf, NULL);
	    output_asm_insn ("movb (%1), (%0)", operands);
	    sprintf (buf, "\nmovestrhi%d:", count);
	    output_asm_insn (buf, NULL);
	    count++;
	  }
      }
	     
    return "";
}

/* This function checks whether a real value can be encoded as
   a literal, i.e., addressing mode 27.  In that mode, real values
   are one word values, so the remaining 48 bits have to be zero.  */
int
legitimate_const_double_p (rtx address)
{
  long sval[2];
  REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (address), sval);
  if ((sval[0] & 0xffff) == 0 && sval[1] == 0)
    return 1;
  return 0;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */
static bool
pdp11_can_change_mode_class (machine_mode from,
			     machine_mode to,
			     reg_class_t rclass)
{
  /* Also, FPU registers contain a whole float value and the parts of
     it are not separately accessible.

     So we disallow all mode changes involving FPRs.  */
  if (FLOAT_MODE_P (from) != FLOAT_MODE_P (to))
    return false;
  
  return !reg_classes_intersect_p (FPU_REGS, rclass);
}

/* TARGET_PREFERRED_RELOAD_CLASS

   Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  

loading is easier into LOAD_FPU_REGS than FPU_REGS! */

static reg_class_t
pdp11_preferred_reload_class (rtx x, reg_class_t rclass)
{
  if (rclass == FPU_REGS)
    return LOAD_FPU_REGS;
  if (rclass == ALL_REGS)
    {
      if (FLOAT_MODE_P (GET_MODE (x)))
	return LOAD_FPU_REGS;
      else
	return GENERAL_REGS;
    }
  return rclass;
}

/* TARGET_PREFERRED_OUTPUT_RELOAD_CLASS

   Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  

loading is easier into LOAD_FPU_REGS than FPU_REGS! */

static reg_class_t
pdp11_preferred_output_reload_class (rtx x, reg_class_t rclass)
{
  if (rclass == FPU_REGS)
    return LOAD_FPU_REGS;
  if (rclass == ALL_REGS)
    {
      if (FLOAT_MODE_P (GET_MODE (x)))
	return LOAD_FPU_REGS;
      else
	return GENERAL_REGS;
    }
  return rclass;
}


/* TARGET_SECONDARY_RELOAD.

   FPU registers AC4 and AC5 (class NO_LOAD_FPU_REGS) require an 
   intermediate register (AC0-AC3: LOAD_FPU_REGS).  Everything else
   can be loade/stored directly.  */
static reg_class_t 
pdp11_secondary_reload (bool in_p ATTRIBUTE_UNUSED,
			rtx x,
			reg_class_t reload_class,
			machine_mode reload_mode ATTRIBUTE_UNUSED,
			secondary_reload_info *sri ATTRIBUTE_UNUSED)
{
  if (reload_class != NO_LOAD_FPU_REGS || GET_CODE (x) != REG ||
      REGNO_REG_CLASS (REGNO (x)) == LOAD_FPU_REGS)
    return NO_REGS;
  
  return LOAD_FPU_REGS;
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED.

   The answer is yes if we're going between general register and FPU 
   registers.  The mode doesn't matter in making this check.
*/
static bool
pdp11_secondary_memory_needed (machine_mode, reg_class_t c1, reg_class_t c2)
{
  int fromfloat = (c1 == LOAD_FPU_REGS || c1 == NO_LOAD_FPU_REGS || 
		   c1 == FPU_REGS);
  int tofloat = (c2 == LOAD_FPU_REGS || c2 == NO_LOAD_FPU_REGS || 
		 c2 == FPU_REGS);
  
  return (fromfloat != tofloat);
}

/* TARGET_LEGITIMATE_ADDRESS_P recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

*/

static bool
pdp11_legitimate_address_p (machine_mode mode,
			    rtx operand, bool strict)
{
    rtx xfoob;

    /* accept @#address */
    if (CONSTANT_ADDRESS_P (operand))
      return true;
    
    switch (GET_CODE (operand))
      {
      case REG:
	/* accept (R0) */
	return !strict || REGNO_OK_FOR_BASE_P (REGNO (operand));
    
      case PLUS:
	/* accept X(R0) */
	return GET_CODE (XEXP (operand, 0)) == REG
	  && (!strict || REGNO_OK_FOR_BASE_P (REGNO (XEXP (operand, 0))))
	  && CONSTANT_ADDRESS_P (XEXP (operand, 1));

      case PRE_DEC:
	/* accept -(R0) */
	return GET_CODE (XEXP (operand, 0)) == REG
	  && (!strict || REGNO_OK_FOR_BASE_P (REGNO (XEXP (operand, 0))));

      case POST_INC:
	/* accept (R0)+ */
	return GET_CODE (XEXP (operand, 0)) == REG
	  && (!strict || REGNO_OK_FOR_BASE_P (REGNO (XEXP (operand, 0))));

      case PRE_MODIFY:
	/* accept -(SP) -- which uses PRE_MODIFY for byte mode */
	return GET_CODE (XEXP (operand, 0)) == REG
	  && REGNO (XEXP (operand, 0)) == STACK_POINTER_REGNUM
	  && GET_CODE ((xfoob = XEXP (operand, 1))) == PLUS
	  && GET_CODE (XEXP (xfoob, 0)) == REG
	  && REGNO (XEXP (xfoob, 0)) == STACK_POINTER_REGNUM
	  && CONSTANT_P (XEXP (xfoob, 1))
	  && INTVAL (XEXP (xfoob,1)) == -2;

      case POST_MODIFY:
	/* accept (SP)+ -- which uses POST_MODIFY for byte mode */
	return GET_CODE (XEXP (operand, 0)) == REG
	  && REGNO (XEXP (operand, 0)) == STACK_POINTER_REGNUM
	  && GET_CODE ((xfoob = XEXP (operand, 1))) == PLUS
	  && GET_CODE (XEXP (xfoob, 0)) == REG
	  && REGNO (XEXP (xfoob, 0)) == STACK_POINTER_REGNUM
	  && CONSTANT_P (XEXP (xfoob, 1))
	  && INTVAL (XEXP (xfoob,1)) == 2;

      case MEM:
	/* handle another level of indirection ! */
	xfoob = XEXP (operand, 0);

	/* (MEM:xx (MEM:xx ())) is not valid for SI, DI and currently
	   also forbidden for float, because we have to handle this 
	   in output_move_double and/or output_move_quad() - we could
	   do it, but currently it's not worth it!!! 
	   now that DFmode cannot go into CPU register file, 
	   maybe I should allow float ... 
	   but then I have to handle memory-to-memory moves in movdf ??  */
	if (GET_MODE_BITSIZE(mode) > 16)
	  return false;

	/* accept @address */
	if (CONSTANT_ADDRESS_P (xfoob))
	  return true;

	switch (GET_CODE (xfoob))
	  {
	  case REG:
	    /* accept @(R0) - which is @0(R0) */
	    return !strict || REGNO_OK_FOR_BASE_P(REGNO (xfoob));

	  case PLUS:
	    /* accept @X(R0) */
	    return GET_CODE (XEXP (xfoob, 0)) == REG
	      && (!strict || REGNO_OK_FOR_BASE_P (REGNO (XEXP (xfoob, 0))))
	      && CONSTANT_ADDRESS_P (XEXP (xfoob, 1));

	  case PRE_DEC:
	    /* accept @-(R0) */
	    return GET_CODE (XEXP (xfoob, 0)) == REG
	      && (!strict || REGNO_OK_FOR_BASE_P (REGNO (XEXP (xfoob, 0))));

	  case POST_INC:
	    /* accept @(R0)+ */
	    return GET_CODE (XEXP (xfoob, 0)) == REG
	      && (!strict || REGNO_OK_FOR_BASE_P (REGNO (XEXP (xfoob, 0))));

	  default:
	    /* anything else is invalid */
	    return false;
	  }

      default:
	/* anything else is invalid */
	return false;
      }
}

/* Return the class number of the smallest class containing
   reg number REGNO.  */
enum reg_class
pdp11_regno_reg_class (int regno)
{ 
  if (regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    return GENERAL_REGS;
  else if (regno > AC3_REGNUM)
    return NO_LOAD_FPU_REGS;
  else if (regno >= AC0_REGNUM)
    return LOAD_FPU_REGS;
  else if (regno & 1)
    return MUL_REGS;
  else
    return GENERAL_REGS;
}


int
pdp11_sp_frame_offset (void)
{
  int offset = 0, regno;
  offset = get_frame_size();
  for (regno = 0; regno <= PC_REGNUM; regno++)
    if (pdp11_saved_regno (regno))
      offset += 2;
  for (regno = AC0_REGNUM; regno <= AC5_REGNUM; regno++)
    if (pdp11_saved_regno (regno))
      offset += 8;
  
  return offset;
}   

/* Return the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

int
pdp11_initial_elimination_offset (int from, int to)
{
  int spoff;
  
  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return 4;
  else if (from == FRAME_POINTER_REGNUM
	   && to == HARD_FRAME_POINTER_REGNUM)
    return 0;
  else
    {
      gcc_assert (to == STACK_POINTER_REGNUM);

      /* Get the size of the register save area.  */
      spoff = pdp11_sp_frame_offset ();
      if (from == FRAME_POINTER_REGNUM)
	return spoff;

      gcc_assert (from == ARG_POINTER_REGNUM);

      /* If there is a frame pointer, that is saved too.  */
      if (frame_pointer_needed)
	spoff += 2;
      
      /* Account for the saved PC in the function call.  */
      return spoff + 2;
    }
}    

/* A copy of output_addr_const modified for pdp11 expression syntax.
   output_addr_const also gets called for %cDIGIT and %nDIGIT, which we don't
   use, and for debugging output, which we don't support with this port either.
   So this copy should get called whenever needed.
*/
void
output_addr_const_pdp11 (FILE *file, rtx x)
{
  char buf[256];
  int i;
  
 restart:
  switch (GET_CODE (x))
    {
    case PC:
      gcc_assert (flag_pic);
      putc ('.', file);
      break;

    case SYMBOL_REF:
      assemble_name (file, XSTR (x, 0));
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
      assemble_name (file, buf);
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (file, buf);
      break;

    case CONST_INT:
      i = INTVAL (x);
      if (i < 0)
	{
	  i = -i;
	  fprintf (file, "-");
	}
      fprintf (file, "%#o", i & 0xffff);
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_addr_const_pdp11 (file, XEXP (x, 0));
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %o if the number is one word and positive.  */
	  gcc_assert (!CONST_DOUBLE_HIGH (x));
	  fprintf (file, "%#ho", (unsigned short) CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (e.g. masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  output_addr_const_pdp11 (file, XEXP (x, 1));
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  output_addr_const_pdp11 (file, XEXP (x, 0));
	}
      else
	{
	  output_addr_const_pdp11 (file, XEXP (x, 0));
	  if (INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  output_addr_const_pdp11 (file, XEXP (x, 1));
	}
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      output_addr_const_pdp11 (file, XEXP (x, 0));
      if (GET_CODE (XEXP (x, 1)) != CONST_INT
	  || INTVAL (XEXP (x, 1)) >= 0)
	fprintf (file, "-");
      output_addr_const_pdp11 (file, XEXP (x, 1));
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      output_addr_const_pdp11 (file, XEXP (x, 0));
      break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
pdp11_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  /* Integers 32 bits and under, and scalar floats (if FPU), are returned
     in registers.  The rest go into memory.  */
  return (TYPE_MODE (type) == DImode
	  || (FLOAT_MODE_P (TYPE_MODE (type)) && ! TARGET_AC0)
	  || TREE_CODE (type) == VECTOR_TYPE
	  || COMPLEX_MODE_P (TYPE_MODE (type)));
}

/* Worker function for TARGET_FUNCTION_VALUE.

   On the pdp11 the value is found in R0 (or ac0??? not without FPU!!!! )  */

static rtx
pdp11_function_value (const_tree valtype, 
 		      const_tree fntype_or_decl ATTRIBUTE_UNUSED,
 		      bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype),
		      BASE_RETURN_VALUE_REG(TYPE_MODE(valtype)));
}

/* Worker function for TARGET_LIBCALL_VALUE.  */

static rtx
pdp11_libcall_value (machine_mode mode,
                     const_rtx fun ATTRIBUTE_UNUSED)
{
  return  gen_rtx_REG (mode, BASE_RETURN_VALUE_REG(mode));
}

/* Worker function for TARGET_FUNCTION_VALUE_REGNO_P.

   On the pdp, the first "output" reg is the only register thus used.

   maybe ac0 ? - as option someday!  */

static bool
pdp11_function_value_regno_p (const unsigned int regno)
{
  return (regno == RETVAL_REGNUM) || (TARGET_AC0 && (regno == AC0_REGNUM));
}

/* Worker function for TARGET_TRAMPOLINE_INIT.

   trampoline - how should i do it in separate i+d ? 
   have some allocate_trampoline magic??? 

   the following should work for shared I/D:

   MOV	#STATIC, $4	01270Y	0x0000 <- STATIC; Y = STATIC_CHAIN_REGNUM
   JMP	@#FUNCTION	000137  0x0000 <- FUNCTION
*/

static void
pdp11_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem;

  gcc_assert (!TARGET_SPLIT);

  mem = adjust_address (m_tramp, HImode, 0);
  emit_move_insn (mem, GEN_INT (012700+STATIC_CHAIN_REGNUM));
  mem = adjust_address (m_tramp, HImode, 2);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, HImode, 4);
  emit_move_insn (mem, GEN_INT (000137));
  emit_move_insn (mem, fnaddr);
}

/* Worker function for TARGET_FUNCTION_ARG.

   Determine where to put an argument to a function.
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
pdp11_function_arg (cumulative_args_t cum ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED,
		    const_tree type ATTRIBUTE_UNUSED,
		    bool named ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

/* Worker function for TARGET_FUNCTION_ARG_ADVANCE.

   Update the data in CUM to advance over an argument of mode MODE and
   data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.)  */

static void
pdp11_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			    const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum += (mode != BLKmode
	   ? GET_MODE_SIZE (mode)
	   : int_size_in_bytes (type));
}

/* Make sure everything's fine if we *don't* have an FPU.
   This assumes that putting a register in fixed_regs will keep the
   compiler's mitts completely off it.  We don't bother to zero it out
   of register classes.  Also fix incompatible register naming with
   the UNIX assembler.  */

static void
pdp11_conditional_register_usage (void)
{
  int i;
  HARD_REG_SET x;
  if (!TARGET_FPU)
    {
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FPU_REGS]);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ )
       if (TEST_HARD_REG_BIT (x, i))
	fixed_regs[i] = call_used_regs[i] = 1;
    }

  if (TARGET_AC0)
      call_used_regs[AC0_REGNUM] = 1;
  if (TARGET_UNIX_ASM)
    {
      /* Change names of FPU registers for the UNIX assembler.  */
      reg_names[8] = "fr0";
      reg_names[9] = "fr1";
      reg_names[10] = "fr2";
      reg_names[11] = "fr3";
      reg_names[12] = "fr4";
      reg_names[13] = "fr5";
    }
}

static section *
pdp11_function_section (tree decl ATTRIBUTE_UNUSED,
			enum node_frequency freq ATTRIBUTE_UNUSED,
			bool startup ATTRIBUTE_UNUSED,
			bool exit ATTRIBUTE_UNUSED)
{
  return NULL;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */

static bool
pdp11_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return GET_CODE (x) != CONST_DOUBLE || legitimate_const_double_p (x);
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */

static bool
pdp11_scalar_mode_supported_p (scalar_mode mode)
{
  /* Support SFmode even with -mfloat64.  */
  if (mode == SFmode)
    return true;
  return default_scalar_mode_supported_p (mode);
}

int
pdp11_branch_cost ()
{
  return (TARGET_BRANCH_CHEAP ? 0 : 1);
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
pdp11_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (regno <= PC_REGNUM)
    return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
  return 1;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  On the pdp, the cpu registers
   can hold any mode other than float (because otherwise we may end up
   being asked to move from CPU to FPU register, which isn't a valid
   operation on the PDP11).  For CPU registers, check alignment.

   FPU accepts SF and DF but actually holds a DF - simplifies life!  */

static bool
pdp11_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (regno <= PC_REGNUM)
    return (GET_MODE_BITSIZE (mode) <= 16
	    || (GET_MODE_BITSIZE (mode) >= 32
		&& !(regno & 1)
		&& !FLOAT_MODE_P (mode)));

  return FLOAT_MODE_P (mode);
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
pdp11_modes_tieable_p (machine_mode, machine_mode)
{
  return false;
}

struct gcc_target targetm = TARGET_INITIALIZER;
