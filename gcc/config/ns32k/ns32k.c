/* Subroutines for assembler code output on the NS32000.
   Copyright (C) 1988, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "flags.h"
#include "recog.h"
#include "tm_p.h"

#ifdef OSF_OS
int ns32k_num_files = 0;
#endif

/* This duplicates reg_class_contents in reg_class.c, but maybe that isn't
   initialized in time. Also this is more convenient as an array of ints.
   We know that HARD_REG_SET fits in an unsigned int */

unsigned int ns32k_reg_class_contents[N_REG_CLASSES][1] = REG_CLASS_CONTENTS;

enum reg_class regclass_map[FIRST_PSEUDO_REGISTER] =
{
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  FLOAT_REG0, LONG_FLOAT_REG0, FLOAT_REGS, FLOAT_REGS,
  FLOAT_REGS, FLOAT_REGS, FLOAT_REGS, FLOAT_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FRAME_POINTER_REG, STACK_POINTER_REG
};

const char *const ns32k_out_reg_names[] = OUTPUT_REGISTER_NAMES;

static rtx gen_indexed_expr PARAMS ((rtx, rtx, rtx));
static const char *singlemove_string PARAMS ((rtx *));
static void move_tail PARAMS ((rtx[], int, int));

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */ 
int
hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  int size = GET_MODE_UNIT_SIZE (mode);

  if (FLOAT_MODE_P (mode))
    {
      if (size == UNITS_PER_WORD && regno < L1_REGNUM)
	return 1;
      if (size == UNITS_PER_WORD * 2
	  && (((regno & 1) == 0 && regno < FRAME_POINTER_REGNUM)))
	return 1;
      return 0;
    }
  if (size == UNITS_PER_WORD * 2
      && (regno & 1) == 0 && regno < F0_REGNUM)
    return 1;
  if (size <= UNITS_PER_WORD
      && (regno < F0_REGNUM || regno == FRAME_POINTER_REGNUM
	  || regno == STACK_POINTER_REGNUM))
    return 1;
  return 0;
}

int register_move_cost (CLASS1, CLASS2)
     enum reg_class CLASS1;
     enum reg_class CLASS2;
{
  if (CLASS1 == NO_REGS || CLASS2 == NO_REGS)
    return 2;
  if ((SUBSET_P (CLASS1, FP_REGS) && !SUBSET_P (CLASS2, FP_REGS))
   || (!SUBSET_P (CLASS1, FP_REGS) && SUBSET_P (CLASS2, FP_REGS)))
    return 8;
  if (((CLASS1) == STACK_POINTER_REG && !SUBSET_P (CLASS2,GENERAL_REGS))
      || ((CLASS2) == STACK_POINTER_REG && !SUBSET_P (CLASS1,GENERAL_REGS)))
    return 6;
  if (((CLASS1) == FRAME_POINTER_REG && !SUBSET_P (CLASS2,GENERAL_REGS))
      || ((CLASS2) == FRAME_POINTER_REG && !SUBSET_P (CLASS1,GENERAL_REGS)))
    return 6;
  return 2;
}

#if 0
/* We made the insn definitions copy from floating point to general
  registers via the stack. */
int secondary_memory_needed (CLASS1, CLASS2, M)
     enum reg_class CLASS1;
     enum reg_class CLASS2;
     enum machine_mode M;
{
  int ret = ((SUBSET_P (CLASS1, FP_REGS) && !SUBSET_P (CLASS2, FP_REGS))
   || (!SUBSET_P (CLASS1, FP_REGS) && SUBSET_P (CLASS2, FP_REGS)));
  return ret;
}
#endif
    

/* ADDRESS_COST calls this.  This function is not optimal
   for the 32032 & 32332, but it probably is better than
   the default. */

int
calc_address_cost (operand)
     rtx operand;
{
  int i;
  int cost = 0;
  if (GET_CODE (operand) == MEM)
    cost += 3;
  if (GET_CODE (operand) == MULT)
    cost += 2;
  switch (GET_CODE (operand))
    {
    case REG:
      cost += 1;
      break;
    case POST_DEC:
    case PRE_DEC:
      break;
    case CONST_INT:
      if (INTVAL (operand) <= 7 && INTVAL (operand) >= -8)
	break;
      if (INTVAL (operand) < 0x2000 && INTVAL (operand) >= -0x2000)
	{
	  cost +=1;
	  break;
	}
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      cost +=3;
      break;
    case CONST_DOUBLE:
      cost += 5;
      break;
    case MEM:
      cost += calc_address_cost (XEXP (operand, 0));
      break;
    case MULT:
    case PLUS:
      for (i = 0; i < GET_RTX_LENGTH (GET_CODE (operand)); i++)
	{
	  cost += calc_address_cost (XEXP (operand, i));
	}
    default:
      break;
    }
  return cost;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx in;
{
  int regno = true_regnum (in);

  if (regno >= FIRST_PSEUDO_REGISTER)
    regno = -1;

  if ((class == FRAME_POINTER_REG && regno == STACK_POINTER_REGNUM)
      || ( class == STACK_POINTER_REG && regno == FRAME_POINTER_REGNUM))
    return GENERAL_REGS;
  else
    return NO_REGS;
}

/* Generate the rtx that comes from an address expression in the md file */
/* The expression to be build is BASE[INDEX:SCALE].  To recognize this,
   scale must be converted from an exponent (from ASHIFT) to a
   multiplier (for MULT). */

static rtx
gen_indexed_expr (base, index, scale)
     rtx base, index, scale;
{
  rtx addr;

  /* This generates an invalid addressing mode, if BASE is
     fp or sp.  This is handled by PRINT_OPERAND_ADDRESS.  */
  if (GET_CODE (base) != REG && GET_CODE (base) != CONST_INT)
    base = gen_rtx_MEM (SImode, base);
  addr = gen_rtx_MULT (SImode, index,
		       GEN_INT (1 << INTVAL (scale)));
  addr = gen_rtx_PLUS (SImode, base, addr);
  return addr;
}


/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands". */

void
split_di (operands, num, lo_half, hi_half)
     rtx operands[];
     int num;
     rtx lo_half[], hi_half[];
{
  while (num--)
    {
      if (GET_CODE (operands[num]) == REG)
	{
	  lo_half[num] = gen_rtx_REG (SImode, REGNO (operands[num]));
	  hi_half[num] = gen_rtx_REG (SImode, REGNO (operands[num]) + 1);
	}
      else if (CONSTANT_P (operands[num]))
	{
	  split_double (operands[num], &lo_half[num], &hi_half[num]);
	}
      else if (offsettable_memref_p (operands[num]))
	{
	  lo_half[num] = operands[num];
	  hi_half[num] = adj_offsettable_operand (operands[num], 4);
	}
      else
	abort ();
    }
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static const char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) <= 7
      && INTVAL (operands[1]) >= -8)
    return "movqd %1,%0";
  return "movd %1,%0";
}

const char *
output_move_double (operands)
     rtx *operands;
{
  enum anon1 { REGOP, OFFSOP, PUSHOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1])
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If insn is effectively movd N(sp),tos then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4(sp))
     for the low word as well, to compensate for the first decrement of sp.
     Given this, it doesn't matter which half we do "first".  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = latehalf[1];

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */
  else if (optype0 == PUSHOP || optype1 == PUSHOP)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  /* Loading into a register which overlaps a register used in the address.  */
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (operands[0], operands[1]))
    {
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0))
	  && reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  /* If both halves of dest are used in the src memory address,
	     load the destination address into the low reg (operands[0]).
	     Then it works to load latehalf first.  */
	  rtx xops[2];
	  xops[0] = XEXP (operands[1], 0);
	  xops[1] = operands[0];
	  output_asm_insn ("addr %a0,%1", xops);
	  operands[1] = gen_rtx_MEM (DImode, operands[0]);
	  latehalf[1] = adj_offsettable_operand (operands[1], 4);
	  /* The first half has the overlap, Do the late half first.  */
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  /* Then clobber.  */
	  return singlemove_string (operands);
	}
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0)))
	{
	  /* The first half has the overlap, Do the late half first.  */
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  /* Then clobber.  */
	  return singlemove_string (operands);
	}
    }

  /* Normal case.  Do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  operands[0] = latehalf[0];
  operands[1] = latehalf[1];
  return singlemove_string (operands);
}


#define MAX_UNALIGNED_COPY (32)
/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

static void
move_tail (operands, bytes, offset)
     rtx operands[];
     int bytes;
     int offset;
{
  if (bytes & 2)
    {
      rtx src, dest;
      dest = change_address (operands[0], HImode,
			    plus_constant (XEXP (operands[0], 0), offset));
      src = change_address (operands[1], HImode,
			   plus_constant (XEXP (operands[1], 0), offset));
      emit_move_insn (dest, src);
      offset += 2;
    }
  if (bytes & 1)
    {
      rtx src, dest;
      dest = change_address (operands[0], QImode,
			    plus_constant (XEXP (operands[0], 0), offset));
      src = change_address (operands[1], QImode,
			   plus_constant (XEXP (operands[1], 0), offset));
      emit_move_insn (dest, src);
    }
}

void
expand_block_move (operands)
     rtx operands[];
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int bytes	= (constp ? INTVAL (bytes_rtx) : 0);
  int align	= INTVAL (align_rtx);
  rtx src_reg = gen_rtx_REG (Pmode, 1);
  rtx dest_reg = gen_rtx_REG (Pmode, 2);
  rtx count_reg = gen_rtx_REG (SImode, 0);

  if (constp && bytes <= 0)
    return;

  if (constp && bytes < 20)
    {
      int words = bytes >> 2;
      if (words)
	{
	  if (words < 3 || flag_unroll_loops)
	    {
	      int offset = 0;
	      for (; words; words--, offset += 4)
		{
		  rtx src, dest;
		  dest = change_address (operands[0], SImode,
					plus_constant (XEXP (operands[0], 0), offset));
		  src = change_address (operands[1], SImode,
				       plus_constant (XEXP (operands[1], 0), offset));
		  emit_move_insn (dest, src);
		}
	    }
	  else
	    {
	      /* Use movmd. It is slower than multiple movd's but more
		 compact. It is also slower than movsd for large copies
		 but causes less registers reloading so is better than movsd
		 for small copies. */
	      rtx src, dest;
	      dest = copy_addr_to_reg (XEXP (operands[0], 0));
	      src = copy_addr_to_reg (XEXP (operands[1], 0));
	    
	      emit_insn (gen_movstrsi2(dest, src, GEN_INT (words)));
	    }
	}
      move_tail (operands, bytes & 3, bytes & ~3);
      return;
    }

  if (align > UNITS_PER_WORD)
    align = UNITS_PER_WORD;

  /* Move the address into scratch registers.  */
  emit_insn (gen_rtx_CLOBBER (VOIDmode, dest_reg));
  emit_move_insn (dest_reg, XEXP (operands[0], 0));
  operands[0] = gen_rtx_MEM (SImode, dest_reg);
  emit_insn (gen_rtx_CLOBBER (VOIDmode, src_reg));
  emit_move_insn (src_reg, XEXP (operands[1], 0));
  operands[1] = gen_rtx_MEM (SImode, src_reg);
  emit_insn (gen_rtx_CLOBBER (VOIDmode, count_reg));

  if (constp && (align == UNITS_PER_WORD || bytes < MAX_UNALIGNED_COPY))
    {
      /* constant no of bytes and aligned or small enough copy to not bother
       * aligning. Emit insns to copy by words.
       */
      if (bytes >> 2)
	{
	  emit_move_insn (count_reg, GEN_INT (bytes >> 2));
	  emit_insn (gen_movstrsi1 (GEN_INT (4)));
	}
      /* insns to copy rest */
      move_tail (operands, bytes & 3, 0);
    }
  else if (align == UNITS_PER_WORD)
    {
      /* insns to copy by words */
      emit_insn (gen_lshrsi3 (count_reg, bytes_rtx, GEN_INT (2)));
      emit_insn (gen_movstrsi1 (GEN_INT (4)));
      if (constp)
	{
	  move_tail (operands, bytes & 3, 0);
	}
      else
	{
	  /* insns to copy rest */
	  emit_insn (gen_andsi3 (count_reg, bytes_rtx, GEN_INT (3)));
	  emit_insn (gen_movstrsi1 (const1_rtx));
	}
    }
  else
    {
      /* Not aligned and we may have a lot to copy so it is worth
       * aligning.
       */
      rtx aligned_label = gen_label_rtx ();
      rtx bytes_reg;

      bytes_reg = copy_to_mode_reg (SImode, bytes_rtx);
      if (!constp)
	{
	  /* Emit insns to test and skip over the alignment if it is
	   * not worth it. This doubles as a test to ensure that the alignment
	   * operation can't copy too many bytes
	   */
	  emit_insn (gen_cmpsi (bytes_reg, GEN_INT (MAX_UNALIGNED_COPY)));
	  emit_jump_insn (gen_blt (aligned_label));
	}

      /* Emit insns to do alignment at run time */
      emit_insn (gen_negsi2 (count_reg, src_reg));
      emit_insn (gen_andsi3 (count_reg, count_reg, GEN_INT (3)));
      emit_insn (gen_subsi3 (bytes_reg, bytes_reg, count_reg));
      emit_insn (gen_movstrsi1 (const1_rtx));
      if (!constp)
	emit_label (aligned_label);

      /* insns to copy by words */
      emit_insn (gen_lshrsi3 (count_reg, bytes_reg, GEN_INT (2)));
      emit_insn (gen_movstrsi1 (GEN_INT (4)));

      /* insns to copy rest */
      emit_insn (gen_andsi3 (count_reg, bytes_reg, GEN_INT (3)));
      emit_insn (gen_movstrsi1 (const1_rtx));
    }
}


/* Returns 1 if OP contains a global symbol reference */

int
global_symbolic_reference_mentioned_p (op, f)
     rtx op;
     int f;
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF)
    {
      if (! SYMBOL_REF_FLAG (op))
	return 1;
      else
        return 0;
    }
  else if (f && GET_CODE (op) != CONST)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (global_symbolic_reference_mentioned_p (XVECEXP (op, i, j), 0))
	      return 1;
	}
      else if (fmt[i] == 'e' 
	       && global_symbolic_reference_mentioned_p (XEXP (op, i), 0))
	return 1;
    }

  return 0;
}


/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (op)
     rtx op;
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Return nonzero if IDENTIFIER with arguments ARGS is a valid machine specific
   attribute for DECL.  The attributes in ATTRIBUTES have previously been
   assigned to DECL.  */

int
ns32k_valid_decl_attribute_p (decl, attributes, identifier, args)
     tree decl ATTRIBUTE_UNUSED;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier ATTRIBUTE_UNUSED;
     tree args ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Return nonzero if IDENTIFIER with arguments ARGS is a valid machine specific
   attribute for TYPE.  The attributes in ATTRIBUTES have previously been
   assigned to TYPE.  */

int
ns32k_valid_type_attribute_p (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != FIELD_DECL
      && TREE_CODE (type) != TYPE_DECL)
    return 0;

  /* Stdcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  if (is_attribute_p ("stdcall", identifier))
    return (args == NULL_TREE);

  /* Cdecl attribute says the callee is a normal C declaration */
  if (is_attribute_p ("cdecl", identifier))
    return (args == NULL_TREE);

  return 0;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

int
ns32k_comp_type_attributes (type1, type2)
     tree type1 ATTRIBUTE_UNUSED;
     tree type2 ATTRIBUTE_UNUSED;
{
  return 1;
}


/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the ns32k, the RET insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RET can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RET is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RET on a per module basis.  */

int
ns32k_return_pops_args (fundecl, funtype, size)
     tree fundecl ATTRIBUTE_UNUSED;
     tree funtype;
     int size;
{
  int rtd = TARGET_RTD;

  if (TREE_CODE (funtype) == IDENTIFIER_NODE)
    return rtd ? size : 0;

  /* Cdecl functions override -mrtd, and never pop the stack */
  if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (funtype)))
    return 0;

  /* Stdcall functions will pop the stack if not variable args */
  if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (funtype)))
    rtd = 1;

  if (rtd)
    {
      if (TYPE_ARG_TYPES (funtype) == NULL_TREE
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (funtype))) == void_type_node))
	return size;
    }

  return 0;
}

/* PRINT_OPERAND is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in ns32k.h.  */

/* XXX time 12% of cpu time is in fprintf for non optimizing */
void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  if (code == '$')
    PUT_IMMEDIATE_PREFIX (file);
  else if (code == '?')
    PUT_EXTERNAL_PREFIX (file);
  else if (GET_CODE (x) == REG)
    fprintf (file, "%s", ns32k_out_reg_names[REGNO (x)]);
  else if (GET_CODE (x) == MEM)
    {
      output_address (XEXP (x, 0));
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != VOIDmode)
    {
      if (GET_MODE (x) == DFmode)
	{ 
	  union { double d; int i[2]; } u;
	  u.i[0] = CONST_DOUBLE_LOW (x); u.i[1] = CONST_DOUBLE_HIGH (x);
	  PUT_IMMEDIATE_PREFIX (file);
#ifdef SEQUENT_ASM
	  /* Sequent likes its floating point constants as integers */
	  fprintf (file, "0Dx%08x%08x", u.i[1], u.i[0]);
#else
#ifdef ENCORE_ASM
	  fprintf (file, "0f%.20e", u.d); 
#else
	  fprintf (file, "0d%.20e", u.d); 
#endif
#endif
	}
      else
	{ 
	  union { double d; int i[2]; } u;
	  u.i[0] = CONST_DOUBLE_LOW (x); u.i[1] = CONST_DOUBLE_HIGH (x);
	  PUT_IMMEDIATE_PREFIX (file);
#ifdef SEQUENT_ASM
	  /* We have no way of winning if we can't get the bits
	     for a sequent floating point number.  */
#if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
	  abort ();
#endif
	  {
	    union { float f; long l; } uu;
	    uu.f = u.d;
	    fprintf (file, "0Fx%08x", uu.l);
	  }
#else
	  fprintf (file, "0f%.20e", u.d); 
#endif
	}
    }
  else
    {
      if (flag_pic
          && GET_CODE (x) == CONST
          && symbolic_reference_mentioned_p (x))
        {
	  fprintf (stderr, "illegal constant for pic-mode: \n");
	  print_rtl (stderr, x);
          fprintf (stderr, "\nGET_CODE (x) == %d, CONST == %d, symbolic_reference_mentioned_p (x) == %d\n",
		  GET_CODE (x), CONST, symbolic_reference_mentioned_p (x));
	  abort ();
	}
      else if (flag_pic
               && (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF))
	{
	  output_addr_const (file, x);
	  fprintf (file, "(sb)");
	}
      else
        {
#ifdef NO_IMMEDIATE_PREFIX_IF_SYMBOLIC
          if (GET_CODE (x) == CONST_INT)
#endif
	    PUT_IMMEDIATE_PREFIX (file);
          output_addr_const (file, x);
	}
    }
}

/* PRINT_OPERAND_ADDRESS is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in ns32k.h .  */

/* Completely rewritten to get this to work with Gas for PC532 Mach.
   This function didn't work and I just wasn't able (nor very willing) to
   figure out how it worked.
   90-11-25 Tatu Yl|nen <ylo@cs.hut.fi> */

void
print_operand_address (file, addr)
     register FILE *file;
     register rtx addr;
{
  static char scales[] = { 'b', 'w', 'd', 0, 'q', };
  rtx offset, base, indexexp, tmp;
  int scale;
  extern int flag_pic;

  if (GET_CODE (addr) == PRE_DEC || GET_CODE (addr) == POST_DEC)
    {
      fprintf (file, "tos");
      return;
    }

  offset = NULL;
  base = NULL;
  indexexp = NULL;
  while (addr != NULL)
    {
      if (GET_CODE (addr) == PLUS)
	{
	  if (GET_CODE (XEXP (addr, 0)) == PLUS)
	    {
	      tmp = XEXP (addr, 1);
	      addr = XEXP (addr, 0);
	    }
	  else
	    {
	      tmp = XEXP (addr,0);
	      addr = XEXP (addr,1);
	    }
	}
      else
	{
	  tmp = addr;
	  addr = NULL;
	}
      switch (GET_CODE (tmp))
	{
	case PLUS:
	  abort ();
	case MEM:
	  if (base)
	    {
	      indexexp = base;
	      base = tmp;
	    }
	  else
	    base = tmp;
	  break;
	case REG:
	  if (REGNO (tmp) < F0_REGNUM)
	    if (base)
	      {
		indexexp = tmp;
	      }
	    else
	      base = tmp;
	  else
	    if (base)
	      {
		indexexp = base;
		base = tmp;
	      }
	    else
	      base = tmp;
	  break;
	case MULT:
	  indexexp = tmp;
	  break;
	case SYMBOL_REF:
	  if (flag_pic && ! CONSTANT_POOL_ADDRESS_P (tmp)
	      && ! SYMBOL_REF_FLAG (tmp))
	    {
	      if (base)
		{
		  if (indexexp)
		    abort ();
		  indexexp = base;
		}
	      base = tmp;
	      break;
	    }
	case CONST:
	  if (flag_pic && GET_CODE (tmp) == CONST)
	    {
	      rtx sym, off, tmp1;
	      tmp1 = XEXP (tmp,0);
	      if (GET_CODE (tmp1)  != PLUS)
		abort ();

	      sym = XEXP (tmp1,0);
	      if (GET_CODE (sym) != SYMBOL_REF)
	        {
	          off = sym;
		  sym = XEXP (tmp1,1);
		}
	      else
	        off = XEXP (tmp1,1);
	      if (GET_CODE (sym) == SYMBOL_REF)
		{
		  if (GET_CODE (off) != CONST_INT)
		    abort ();

		  if (CONSTANT_POOL_ADDRESS_P (sym)
		      || SYMBOL_REF_FLAG (sym))
		    {
		      SYMBOL_REF_FLAG (tmp) = 1;
		    }
		  else
		    {
		      if (base)
			{
			  if (indexexp)
			    abort ();

			  indexexp = base;
			}

		      if (offset != 0)
			abort ();

		      base = sym;
		      offset = off;
		      break;
		    }
		}
	    }
	case CONST_INT:
	case LABEL_REF:
	  if (offset)
	    offset = gen_rtx_PLUS (SImode, tmp, offset);
	  else
	    offset = tmp;
	  break;
	default:
	  abort ();
	}
    }
  if (! offset)
    offset = const0_rtx;

  if (base
#ifndef INDEX_RATHER_THAN_BASE
      && (flag_pic || TARGET_HIMEM)
      && GET_CODE (base) != SYMBOL_REF 
      && GET_CODE (offset) != CONST_INT
#else
  /* This is a re-implementation of the SEQUENT_ADDRESS_BUG fix.  */
#endif
      && !indexexp && GET_CODE (base) == REG
      && REG_OK_FOR_INDEX_P (base))
    {
      indexexp = base;
      base = NULL;
    }

  /* now, offset, base and indexexp are set */
#ifndef BASE_REG_NEEDED
  if (! base)
    {
#if defined (PC_RELATIVE) || defined (NO_ABSOLUTE_PREFIX_IF_SYMBOLIC)
      if (GET_CODE (offset) == CONST_INT)
#endif
	PUT_ABSOLUTE_PREFIX (file);
    }
#endif

  output_addr_const (file, offset);
  if (base) /* base can be (REG ...) or (MEM ...) */
    switch (GET_CODE (base))
      {
	/* now we must output base.  Possible alternatives are:
	   (rN)       (REG ...)
	   (sp)	      (REG ...)
	   (fp)       (REG ...)
	   (pc)       (REG ...)  used for SYMBOL_REF and LABEL_REF, output
	   (disp(fp)) (MEM ...)       just before possible [rX:y]
	   (disp(sp)) (MEM ...)
	   (disp(sb)) (MEM ...)
	   */
      case REG:
	fprintf (file, "(%s)", ns32k_out_reg_names[REGNO (base)]);
	break;
      case SYMBOL_REF:
	if (! flag_pic)
	  abort ();

        fprintf (file, "(");
	output_addr_const (file, base);
	fprintf (file, "(sb))");
        break;
      case MEM:
	addr = XEXP (base,0);
	base = NULL;
	offset = NULL;
	while (addr != NULL)
	  {
	    if (GET_CODE (addr) == PLUS)
	      {
		if (GET_CODE (XEXP (addr, 0)) == PLUS)
		  {
		    tmp = XEXP (addr, 1);
		    addr = XEXP (addr, 0);
		  }
		else
		  {
		    tmp = XEXP (addr, 0);
		    addr = XEXP (addr, 1);
		  }
	      }
	    else
	      {
		tmp = addr;
		addr = NULL;
	      }
	    switch (GET_CODE (tmp))
	      {
	      case REG:
		base = tmp;
		break;
	      case CONST:
	      case CONST_INT:
	      case SYMBOL_REF:
	      case LABEL_REF:
		if (offset)
		  offset = gen_rtx_PLUS (SImode, tmp, offset);
		else
		  offset = tmp;
		break;
	      default:
		abort ();
	      }
	  }
	if (! offset)
	  offset = const0_rtx;
	fprintf (file, "(");
	output_addr_const (file, offset);
	if (base)
	  fprintf (file, "(%s)", ns32k_out_reg_names[REGNO (base)]);
	else if (TARGET_SB)
	  fprintf (file, "(sb)");
	else
	  abort ();
	fprintf (file, ")");
	break;
      default:
	abort ();
      }
#ifdef PC_RELATIVE
  else if (GET_CODE (offset) != CONST_INT)
    fprintf (file, "(pc)");
#ifdef BASE_REG_NEEDED
  else if (TARGET_SB)
    fprintf (file, "(sb)");
  else
    abort ();
#endif
#endif /* PC_RELATIVE */

  /* now print index if we have one */
  if (indexexp)
    {
      if (GET_CODE (indexexp) == MULT)
	{
	  scale = INTVAL (XEXP (indexexp, 1)) >> 1;
	  indexexp = XEXP (indexexp, 0);
	}
      else
	scale = 0;
      if (GET_CODE (indexexp) != REG || REGNO (indexexp) >= F0_REGNUM)
	abort ();

#ifdef UTEK_ASM
      fprintf (file, "[%c`%s]",
	       scales[scale],
	       ns32k_out_reg_names[REGNO (indexexp)]);
#else
      fprintf (file, "[%s:%c]",
	       ns32k_out_reg_names[REGNO (indexexp)],
	       scales[scale]);
#endif
    }
}

/* National 32032 shifting is so bad that we can get
   better performance in many common cases by using other
   techniques.  */
const char *
output_shift_insn (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) <= 3)
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  if (GET_CODE (operands[1]) == REG)
	    {
	      if (REGNO (operands[0]) == REGNO (operands[1]))
		{
		  if (operands[2] == const1_rtx)
		    return "addd %0,%0";
		  else if (INTVAL (operands[2]) == 2)
		    return "addd %0,%0\n\taddd %0,%0";
		}
	      if (operands[2] == const1_rtx)
		return "movd %1,%0\n\taddd %0,%0";
	    
	      operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	      return "addr %a1,%0";
	    }
	  if (operands[2] == const1_rtx)
	    return "movd %1,%0\n\taddd %0,%0";
	}
      else if (GET_CODE (operands[1]) == REG)
	{
	  operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	  return "addr %a1,%0";
	}
      else if (INTVAL (operands[2]) == 1
	       && GET_CODE (operands[1]) == MEM
	       && rtx_equal_p (operands [0], operands[1]))
	{
	  rtx temp = XEXP (operands[1], 0);
	
	  if (GET_CODE (temp) == REG
	      || (GET_CODE (temp) == PLUS
		  && GET_CODE (XEXP (temp, 0)) == REG
		  && GET_CODE (XEXP (temp, 1)) == CONST_INT))
	    return "addd %0,%0";
	}
      else return "ashd %2,%0";
    }
  return "ashd %2,%0";
}

const char *
output_move_dconst (n, s)
	int n;
	const char *s;
{
  static char r[32];

  if (n > -9 && n < 8)
    strcpy (r, "movqd ");
  else if (n > 0 && n < 256)
    strcpy (r, "movzbd ");
  else if (n > 0 && n < 65536)
    strcpy (r, "movzwd ");
  else if (n < 0 && n > -129)
    strcpy (r, "movxbd ");
  else if (n < 0 && n > -32769)
    strcpy (r, "movxwd ");
  else
    strcpy (r, "movd ");
  strcat (r, s);
  return r;
}
