/* Subroutines used for code generation on IBM S/390 and zSeries
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).

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
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "recog.h"
#include "expr.h"
#include "toplev.h"
#include "basic-block.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"




#undef  TARGET_ASM_FUNCTION_PROLOGUE 
#define TARGET_ASM_FUNCTION_PROLOGUE s390_function_prologue

#undef  TARGET_ASM_FUNCTION_EPILOGUE 
#define TARGET_ASM_FUNCTION_EPILOGUE s390_function_epilogue

#undef  TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN ""

#undef  TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ""

struct gcc_target targetm = TARGET_INITIALIZER;

extern int reload_completed;

/* Function count for creating unique internal labels in a compile unit.  */
int  s390_function_count = 0;

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */
rtx s390_compare_op0, s390_compare_op1;

/* Structure used to hold the components of a S/390 memory
   address.  A legitimate address on S/390 is of the general
   form
          base + index + displacement
   where any of the components is optional.

   base and index are registers of the class ADDR_REGS,
   displacement is an unsigned 12-bit immediate constant.  */

struct s390_address
{
  rtx base;
  rtx indx;
  rtx disp;
};

static int s390_match_ccmode_set PARAMS ((rtx, enum machine_mode));
static int base_n_index_p PARAMS ((rtx));
static int check_mode PARAMS ((rtx, enum machine_mode *));
static int s390_decompose_address PARAMS ((rtx, struct s390_address *, int));
static void output_branch_condition PARAMS ((FILE *, rtx));
static void output_inverse_branch_condition PARAMS ((FILE *, rtx));
static int reg_used_in_mem_p PARAMS ((int, rtx));
static int addr_generation_dependency_p PARAMS ((rtx, rtx));
static int other_chunk PARAMS ((int *, int, int));
static int far_away PARAMS ((int, int));
static rtx check_and_change_labels PARAMS ((rtx, int *));
static void s390_final_chunkify PARAMS ((int));
static int save_fprs_p PARAMS ((void));
static int cur_is_leaf_function PARAMS ((void));
static int save_fprs PARAMS ((FILE *, long, int));
static int restore_fprs PARAMS ((FILE *, long, int));
static void s390_output_constant_pool PARAMS ((FILE *));
static rtx s390_force_const_mem_late PARAMS ((rtx));
static rtx s390_force_const_mem_symbol PARAMS ((const char *, int, int));
static int s390_function_arg_size PARAMS ((enum machine_mode, tree));

 
/* Return true if SET either doesn't set the CC register, or else
   the source and destination have matching CC modes and that 
   CC mode is at least as constrained as REQ_MODE.  */
 
static int
s390_match_ccmode_set (set, req_mode)
     rtx set;
     enum machine_mode req_mode;
{
  enum machine_mode set_mode;

  if (GET_CODE (set) != SET)
    abort ();

  if (GET_CODE (SET_DEST (set)) != REG || !CC_REGNO_P (REGNO (SET_DEST (set))))
    return 1;

  set_mode = GET_MODE (SET_DEST (set));
  switch (set_mode)
    {
    case CCmode:
      return 0;

    case CCSmode:
      if (req_mode != CCSmode)
        return 0;
      break;
    case CCUmode:
      if (req_mode != CCUmode)
        return 0;
      break;
    case CCZmode:
      if (req_mode != CCSmode && req_mode != CCUmode && req_mode != CCTmode)
        return 0;
      break;
 
    default:
      abort ();
    }
 
  return (GET_MODE (SET_SRC (set)) == set_mode);
}

/* Return true if every SET in INSN that sets the CC register 
   has source and destination with matching CC modes and that 
   CC mode is at least as constrained as REQ_MODE.  */
 
int
s390_match_ccmode (insn, req_mode)
     rtx insn;
     enum machine_mode req_mode;
{
  int i;

  if (GET_CODE (PATTERN (insn)) == SET)
    return s390_match_ccmode_set (PATTERN (insn), req_mode);

  if (GET_CODE (PATTERN (insn)) == PARALLEL)
      for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
        {
          rtx set = XVECEXP (PATTERN (insn), 0, i);
          if (GET_CODE (set) == SET)
            if (!s390_match_ccmode_set (set, req_mode))
              return 0;
        }

  return 1;
}

/* Change optimizations to be performed, depending on the 
   optimization level.

   LEVEL is the optimization level specified; 2 if `-O2' is
   specified, 1 if `-O' is specified, and 0 if neither is specified.

   SIZE is non-zero if `-Os' is specified and zero otherwise. */

void
optimization_options (level, size)
     int level ATTRIBUTE_UNUSED;
     int size ATTRIBUTE_UNUSED;
{
#ifdef HAVE_decrement_and_branch_on_count
  /* When optimizing, enable use of BRCT instruction.  */
  if (level >= 1)
      flag_branch_on_count_reg = 1;
#endif
}


/* Map for smallest class containing reg regno.  */

enum reg_class regclass_map[FIRST_PSEUDO_REGISTER] =
{ GENERAL_REGS, ADDR_REGS, ADDR_REGS, ADDR_REGS,
  ADDR_REGS,    ADDR_REGS, ADDR_REGS, ADDR_REGS,
  ADDR_REGS,    ADDR_REGS, ADDR_REGS, ADDR_REGS,
  ADDR_REGS,    ADDR_REGS, ADDR_REGS, ADDR_REGS,
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,
  ADDR_REGS,    NO_REGS 
};


/* Return true if OP a (const_int 0) operand.
   OP is the current operation.
   MODE is the current operation mode.  */
 
int
const0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return op == CONST0_RTX (mode);
}

/* Return true if OP a (const_int 1) operand.
   OP is the current operation.
   MODE is the current operation mode.  */
 
int
const1_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return op == CONST1_RTX (mode);
}

/* Return true if OP needs base and index register.  */

static int 
base_n_index_p (op)
     register rtx op;
{
  if ((GET_CODE (op) == PLUS) &&
      (GET_CODE (XEXP (op, 0)) == PLUS ||
       GET_CODE (XEXP (op, 1)) == PLUS ||
       GET_CODE (XEXP (op, 1)) == REG ))
    return 1;
  return 0;
}

/* Return true if the mode of operand OP matches MODE.
   If MODE is set to VOIDmode, set it to the mode of OP.  */ 

static int
check_mode (op, mode)
     register rtx op;
     enum machine_mode *mode;
{
  if (*mode == VOIDmode)
      *mode = GET_MODE (op);
  else
  {
    if (GET_MODE (op) != VOIDmode && GET_MODE (op) != *mode)
       return 0;
  }
  return 1;
}

/* Return true if OP a valid operand for the LARL instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
larl_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (! check_mode (op, &mode))
    return 0;

  /* Allow labels and local symbols.  */
  if (GET_CODE (op) == LABEL_REF)
    return 1;
  if (GET_CODE (op) == SYMBOL_REF
      && (!flag_pic || SYMBOL_REF_FLAG (op) 
          || CONSTANT_POOL_ADDRESS_P (op)))
    return 1;

  /* Everything else must have a CONST, so strip it.  */
  if (GET_CODE (op) != CONST)
    return 0;
  op = XEXP (op, 0);

  /* Allow adding *even* constants.  */
  if (GET_CODE (op) == PLUS)
    {
      if (GET_CODE (XEXP (op, 1)) != CONST_INT
          || (INTVAL (XEXP (op, 1)) & 1) != 0)
        return 0;
      op = XEXP (op, 0);
    }

  /* Labels and local symbols allowed here as well.  */
  if (GET_CODE (op) == LABEL_REF)
    return 1;
  if (GET_CODE (op) == SYMBOL_REF
      && (!flag_pic || SYMBOL_REF_FLAG (op)
          || CONSTANT_POOL_ADDRESS_P (op)))
    return 1;

  /* Now we must have a @GOTENT offset or @PLT stub.  */
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == 111)
    return 1;
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == 113)
    return 1;

  return 0;
}

/* Return true if OP is a valid FP-Register.
   OP is the current operation.
   MODE is the current operation mode.  */

int
fp_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);
  if (! check_mode (op, &mode))
    return 0;
  if (code == REG && REGNO_OK_FOR_FP_P (REGNO (op)))
    return 1;
  else
    return 0;
}

/* Return true if OP is a valid S operand for an RS, SI or SS type instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
s_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);

  if (! check_mode (op,&mode))
    return 0;

  if (code == MEM) {
    if (base_n_index_p (XEXP (op, 0)))
      return 0;
  }

  return memory_operand (op, mode);
}

/* Return 1 if OP is a valid R or S operand for an RS, SI or SS type
   instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
r_or_s_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);

  if (!general_operand (op, mode))
    return 0;

  if (code == MEM) {
    if (base_n_index_p (XEXP (op, 0)))
      return 0;
    else
      return memory_operand (op, mode);
  }
  return register_operand (op, mode);
}

/* Return true if OP is a valid R or S or immediate operand for 
   RS, SI or SS type instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
r_or_s_or_im8_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);

  if (!general_operand (op, mode))
    return 0;

  if (code == MEM) {
    if (base_n_index_p (XEXP (op, 0)))
      return 0;
    else
      return memory_operand (op, mode);
  }
  return register_operand (op, mode) || immediate_operand (op, mode);
}

/* Return true if OP is a valid R or X or 16 bit immediate operand for 
   RX, RR or RI type instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
r_or_x_or_im16_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{

  if (! general_operand (op, mode))
    return 0;

  if (GET_CODE (op) == CONST_INT)
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'K'));
  return register_operand (op, mode) || memory_operand (op, mode);
}

/* Return true if OP is a valid R or 8 bit immediate operand.
   OP is the current operation.
   MODE is the current operation mode.  */

int
r_or_im8_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{

  if (!general_operand (op, mode))
    return 0;

  if (GET_CODE (op) == CONST_INT)
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'J'));
  return register_operand (op, mode) || memory_operand (op, mode);
}

/* Return true if OP is a valid operand for the 'test under mask'
   instruction with 16 bit immediate.  
   The value should only have set bits in one halfword.
   OP is the current operation.
   MODE is the current operation mode.  */

int
tmxx_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx con;
  if (GET_CODE (op) == CONST_INT)
    return (CONST_OK_FOR_LETTER_P (INTVAL (op), 'L'));
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == SYMBOL_REF &&
      CONSTANT_POOL_ADDRESS_P (XEXP (op, 0))) 
    {
      con = get_pool_constant (XEXP (op, 0));

      if (GET_CODE (con) == CONST_INT)
	{
	  unsigned HOST_WIDEST_INT c;
	  
	  c = (unsigned HOST_WIDEST_INT) INTVAL (con);
	  
	  return ((c & 0xffff) ? ((c & 0xffffffffffff0000ULL)==0) : 
		  (c & 0xffff0000U) ? ((c & 0xffffffff0000ffffULL)==0) :
		  (c & 0xffff00000000ULL) ? ((c & 0xffff0000ffffffffULL)==0) :
		  (c & 0xffff000000000000ULL) ? ((c & 0xffffffffffffULL)==0) : 1);
		  
	}
    }
  return 0;
}

/* Return true if OP is a valid operand for the BRAS instruction.
   OP is the current operation.
   MODE is the current operation mode.  */

int
bras_sym_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  register enum rtx_code code = GET_CODE (op);

  /* Allow SYMBOL_REFs.  */
  if (code == SYMBOL_REF)
    return 1;

  /* Allow @PLT stubs.  */
  if (code == CONST
      && GET_CODE (XEXP (op, 0)) == UNSPEC
      && XINT (XEXP (op, 0), 1) == 113)
    return 1;
  return 0;
}


/* Return true if OP is a load multiple operation.  It is known to be a
   PARALLEL and the first section will be tested. 
   OP is the current operation.
   MODE is the current operation mode.  */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  int i;


  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != Pmode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != Pmode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Return true if OP is a store multiple operation.  It is known to be a
   PARALLEL and the first section will be tested. 
   OP is the current operation.
   MODE is the current operation mode.  */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0) - 1;
  unsigned int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != Pmode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != Pmode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }
  return 1;
}


/* Return true if OP contains a symbol reference */

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


/* Return true if OP is a legitimate general operand when 
   generating PIC code.  It is given that flag_pic is on 
   and that OP satisfies CONSTANT_P or is a CONST_DOUBLE.  */

int
legitimate_pic_operand_p (op)
     register rtx op;
{
  /* All non-symbolic constants that made it 
     up to here are fine.  */
  if (!SYMBOLIC_CONST (op))
    return 1;

  /* Accept immediate LARL operands.  */
  if (TARGET_64BIT)
    return larl_operand (op, VOIDmode);

  /* Reject everything else; must be handled 
     via emit_pic_move.  */
  return 0;
}

/* Returns true if the constant value OP is a legitimate general operand.
   It is given that OP satisfies CONSTANT_P or is a CONST_DOUBLE.  */

int
legitimate_constant_p (op)
     register rtx op;
{
  /* Reject doubles and integers out of range.  */
  if (GET_CODE (op) == CONST_DOUBLE ||
      (GET_CODE (op) == CONST_INT &&
       (INTVAL (op) < -32768 || INTVAL (op) > 32767)))
    return 0;

  /* Accept all other non-symbolic constants.  */
  if (!SYMBOLIC_CONST (op))
    return 1;

  /* In the PIC case, symbolic constants must *not* be
     forced into the literal pool.  We accept them here,
     so that they will be handled by emit_pic_move.  */
  if (flag_pic)
    return 1;

  /* Even in the non-PIC case, we can accept immediate
     LARL operands here.  */
  if (TARGET_64BIT)
    return larl_operand (op, VOIDmode);

  /* All remaining non-PIC symbolic constants are
     forced into the literal pool.  */
  return 0;
}


/* Decompose a RTL expression ADDR for a memory address into
   its components, returned in OUT.  The boolean STRICT 
   specifies whether strict register checking applies.
   Returns 0 if ADDR is not a valid memory address, nonzero
   otherwise.  If OUT is NULL, don't return the components,
   but check for validity only.

   Note: Only addresses in canonical form are recognized.
   LEGITIMIZE_ADDRESS should convert non-canonical forms to the
   canonical form so that they will be recognized.  */

static int
s390_decompose_address (addr, out, strict)
     register rtx addr;
     struct s390_address *out;
     int strict;
{
  rtx base = NULL_RTX;
  rtx indx = NULL_RTX;
  rtx disp = NULL_RTX;

  /* Decompose address into base + index + displacement.  */

  if (GET_CODE (addr) == REG || GET_CODE (addr) == UNSPEC)
    base = addr;

  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      enum rtx_code code0 = GET_CODE (op0);
      enum rtx_code code1 = GET_CODE (op1);

      if (code0 == REG || code0 == UNSPEC)
	{
	  if (code1 == REG || code1 == UNSPEC)
	    {
	      indx = op0;	/* index + base */
	      base = op1;
	    }

	  else
	    {
	      base = op0;	/* base + displacement */
	      disp = op1;
	    }
	}

      else if (code0 == PLUS)
	{
	  indx = XEXP (op0, 0);	/* index + base + disp */
	  base = XEXP (op0, 1);
	  disp = op1;
	}

      else
	{
	  return FALSE;
	}
    }

  else
    disp = addr;		/* displacement */


  /* Validate base register.  */
  if (base)
    {
      if (GET_CODE (base) == UNSPEC)
        {
          if (XVECLEN (base, 0) != 1 || XINT (base, 1) != 101)
	      return FALSE;
	  base = XVECEXP (base, 0, 0);
	}

      if (GET_CODE (base) != REG || GET_MODE (base) != Pmode)
	  return FALSE;

      if ((strict && ! REG_OK_FOR_BASE_STRICT_P (base))
	  || (! strict && ! REG_OK_FOR_BASE_NONSTRICT_P (base)))
	  return FALSE;
    }

  /* Validate index register.  */
  if (indx)
    {
      if (GET_CODE (indx) == UNSPEC)
        {
          if (XVECLEN (indx, 0) != 1 || XINT (indx, 1) != 101)
	      return FALSE;
	  indx = XVECEXP (indx, 0, 0);
	}

      if (GET_CODE (indx) != REG || GET_MODE (indx) != Pmode)
	  return FALSE;

      if ((strict && ! REG_OK_FOR_BASE_STRICT_P (indx))
	  || (! strict && ! REG_OK_FOR_BASE_NONSTRICT_P (indx)))
	  return FALSE;
    }

  /* Validate displacement.  */
  if (disp)
    {
      /* Allow integer constant in range.  */
      if (GET_CODE (disp) == CONST_INT)
        {
          if (INTVAL (disp) < 0 || INTVAL (disp) >= 4096)
              return FALSE;
        }

      /* In the small-PIC case, the linker converts @GOT12 
         offsets to possible displacements.  */
      else if (GET_CODE (disp) == CONST
               && GET_CODE (XEXP (disp, 0)) == UNSPEC
               && XINT (XEXP (disp, 0), 1) == 110)
        {
          if (flag_pic != 1)
            return FALSE;
        }

      /* We can convert literal pool addresses to 
         displacements by basing them off the base register.  */
      else
        {
          /* In some cases, we can accept an additional
             small constant offset.  Split these off here.  */

          unsigned int offset = 0;

          if (GET_CODE (disp) == CONST
              && GET_CODE (XEXP (disp, 0)) == PLUS
              && GET_CODE (XEXP (XEXP (disp, 0), 1)) == CONST_INT)
            {
              offset = INTVAL (XEXP (XEXP (disp, 0), 1));
              disp = XEXP (XEXP (disp, 0), 0);
            }

          /* Now we must have a literal pool address.  */
          if (GET_CODE (disp) != SYMBOL_REF
              || !CONSTANT_POOL_ADDRESS_P (disp))
            return FALSE;

          /* In 64-bit PIC mode we cannot accept symbolic 
             constants in the constant pool.  */
          if (TARGET_64BIT && flag_pic
              && SYMBOLIC_CONST (get_pool_constant (disp)))
            return FALSE;

          /* If we have an offset, make sure it does not
             exceed the size of the constant pool entry.  */
          if (offset && offset >= GET_MODE_SIZE (get_pool_mode (disp)))
            return FALSE;

          /* Either base or index must be free to 
             hold the base register.  */
          if (base && indx)
            return FALSE;

          /* Convert the address.  */
          if (base)
            indx = gen_rtx_REG (Pmode, BASE_REGISTER);
          else
            base = gen_rtx_REG (Pmode, BASE_REGISTER);

          disp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, disp), 100);
          disp = gen_rtx_CONST (Pmode, disp);

          if (offset)
            disp = plus_constant (disp, offset);
        }
    }

  if (out)
    {
      out->base = base;
      out->indx = indx;
      out->disp = disp;
    }

  return TRUE;
}

/* Return nonzero if ADDR is a valid memory address.
   STRICT specifies whether strict register checking applies.  */

int
legitimate_address_p (mode, addr, strict)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     register rtx addr;
     int strict;
{
  return s390_decompose_address (addr, NULL, strict);
}

/* Return a legitimate reference for ORIG (an address) using the
   register REG.  If REG is 0, a new pseudo is generated.

   There are two types of references that must be handled:

   1. Global data references must load the address from the GOT, via
      the PIC reg.  An insn is emitted to do this load, and the reg is
      returned.

   2. Static data references, constant pool addresses, and code labels
      compute the address as an offset from the GOT, whose base is in
      the PIC reg.  Static data objects have SYMBOL_REF_FLAG set to
      differentiate them from global data objects.  The returned
      address is the PIC reg + an unspec constant.

   GO_IF_LEGITIMATE_ADDRESS rejects symbolic references unless the PIC
   reg also appears in the address.  */

rtx
legitimize_pic_address (orig, reg)
     rtx orig;
     rtx reg;
{
  rtx addr = orig;
  rtx new = orig;
  rtx base;

  if (GET_CODE (addr) == LABEL_REF
      || (GET_CODE (addr) == SYMBOL_REF
	  && (SYMBOL_REF_FLAG (addr) 
              || CONSTANT_POOL_ADDRESS_P (addr))))
    {
      /* This is a local symbol.  */
      if (TARGET_64BIT)
        {
          /* Access local symbols PC-relative via LARL.  
             This is the same as in the non-PIC case, so it is 
             handled automatically ... */
        }
      else
        {
          /* Access local symbols relative to the literal pool.  */

          rtx temp = reg? reg : gen_reg_rtx (Pmode);

          addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, addr), 100);
          addr = gen_rtx_CONST (SImode, addr);
          addr = force_const_mem (SImode, addr);
	  emit_move_insn (temp, addr);

          base = gen_rtx_REG (Pmode, BASE_REGISTER);
          base = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, base), 101);
          new = gen_rtx_PLUS (Pmode, base, temp);

          if (reg != 0)
            {
              emit_move_insn (reg, new);
              new = reg;
            }
        }
    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    {
      if (reg == 0)
        reg = gen_reg_rtx (Pmode);

      if (flag_pic == 1)
        {
          /* Assume GOT offset < 4k.  This is handled the same way
             in both 31- and 64-bit code (@GOT12).  */

          current_function_uses_pic_offset_table = 1;

          new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), 110);
          new = gen_rtx_CONST (Pmode, new);
          new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);
          new = gen_rtx_MEM (Pmode, new);
          RTX_UNCHANGING_P (new) = 1;
          emit_move_insn (reg, new);
          new = reg;
        }
      else if (TARGET_64BIT)
        {
          /* If the GOT offset might be >= 4k, we determine the position
             of the GOT entry via a PC-relative LARL (@GOTENT).  */

          rtx temp = gen_reg_rtx (Pmode);

          new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), 111);
          new = gen_rtx_CONST (Pmode, new);
          emit_move_insn (temp, new);

          new = gen_rtx_MEM (Pmode, temp);
          RTX_UNCHANGING_P (new) = 1;
          emit_move_insn (reg, new);
          new = reg;
        }
      else
        {
          /* If the GOT offset might be >= 4k, we have to load it 
             from the literal pool (@GOT).  */

          rtx temp = gen_reg_rtx (Pmode);

          current_function_uses_pic_offset_table = 1;

          addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, addr), 112);
          addr = gen_rtx_CONST (SImode, addr);
          addr = force_const_mem (SImode, addr);
          emit_move_insn (temp, addr);

          new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, temp);
          new = gen_rtx_MEM (Pmode, new);
          RTX_UNCHANGING_P (new) = 1;
          emit_move_insn (reg, new);
          new = reg;
        }
    }      
  else
    {
      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  if (GET_CODE (addr) == UNSPEC)
	    {
	      if (XVECLEN (addr, 0) != 1)
                abort ();
              switch (XINT (addr, 1))
                {
                  /* If someone moved an @GOT or lt-relative UNSPEC
                     out of the literal pool, force them back in.  */
                  case 100:
                  case 112:
                  case 114:
                    new = force_const_mem (SImode, orig);
                    break;

                  /* @GOTENT is OK as is.  */
                  case 111:
                    break;

                  /* @PLT is OK as is on 64-bit, must be converted to
                     lt-relative PLT on 31-bit.  */
                  case 113:
                    if (!TARGET_64BIT)
                      {
                        rtx temp = reg? reg : gen_reg_rtx (Pmode);

                        addr = XVECEXP (addr, 0, 0);
                        addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, addr), 114);
                        addr = gen_rtx_CONST (SImode, addr);
                        addr = force_const_mem (SImode, addr);
	                emit_move_insn (temp, addr);

                        base = gen_rtx_REG (Pmode, BASE_REGISTER);
                        base = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, base), 101);
                        new = gen_rtx_PLUS (Pmode, base, temp);

                        if (reg != 0)
                          {
                            emit_move_insn (reg, new);
                            new = reg;
                          }
                      }
                    break;

                  /* Everything else cannot happen.  */
                  default:
                    abort ();
                }
	    }
	  else if (GET_CODE (addr) != PLUS)
	    abort ();
	}
      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);
	  /* Check first to see if this is a constant offset 
             from a local symbol reference.  */
	  if ((GET_CODE (op0) == LABEL_REF
		|| (GET_CODE (op0) == SYMBOL_REF
		    && (SYMBOL_REF_FLAG (op0)
                        || CONSTANT_POOL_ADDRESS_P (op0))))
	      && GET_CODE (op1) == CONST_INT)
	    {
              if (TARGET_64BIT)
                {
                  if (INTVAL (op1) & 1)
                    {
                      /* LARL can't handle odd offsets, so emit a 
                         pair of LARL and LA.  */
                      rtx temp = reg? reg : gen_reg_rtx (Pmode);

                      if (INTVAL (op1) < 0 || INTVAL (op1) >= 4096)
                        {
                          int even = INTVAL (op1) - 1;
                          op0 = gen_rtx_PLUS (Pmode, op0, GEN_INT (even));
                          op1 = GEN_INT (1);
                        }

                      emit_move_insn (temp, op0);
                      new = gen_rtx_PLUS (Pmode, temp, op1);

                      if (reg != 0)
                        {
                          emit_move_insn (reg, new);
                          new = reg;
                        }
                    }
                  else
                    {
                      /* If the offset is even, we can just use LARL.
                         This will happen automatically.  */
                    }
                }
              else
                {
                  /* Access local symbols relative to the literal pool.  */

                  rtx temp = reg? reg : gen_reg_rtx (Pmode);

                  addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, op0), 100);
                  addr = gen_rtx_PLUS (SImode, addr, op1);
                  addr = gen_rtx_CONST (SImode, addr);
                  addr = force_const_mem (SImode, addr);
        	  emit_move_insn (temp, addr);

                  base = gen_rtx_REG (Pmode, BASE_REGISTER);
                  base = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, base), 101);
                  new = gen_rtx_PLUS (Pmode, base, temp);

                  if (reg != 0)
                    {
                      emit_move_insn (reg, new);
                      new = reg;
                    }
                }
	    }

          /* Now, check whether it is an LT-relative symbol plus offset
             that was pulled out of the literal pool.  Force it back in.  */

	  else if (GET_CODE (op0) == UNSPEC
	           && GET_CODE (op1) == CONST_INT)
            {
	      if (XVECLEN (op0, 0) != 1)
                abort ();
              if (XINT (op0, 1) != 100)
                abort ();

              new = force_const_mem (SImode, orig);
            }

          /* Otherwise, compute the sum.  */
	  else
	    {
	      base = legitimize_pic_address (XEXP (addr, 0), reg);
	      new  = legitimize_pic_address (XEXP (addr, 1),
					     base == reg ? NULL_RTX : reg);
	      if (GET_CODE (new) == CONST_INT)
		new = plus_constant (base, INTVAL (new));
	      else
		{
		  if (GET_CODE (new) == PLUS && CONSTANT_P (XEXP (new, 1)))
		    {
		      base = gen_rtx_PLUS (Pmode, base, XEXP (new, 0));
		      new = XEXP (new, 1);
		    }
		  new = gen_rtx_PLUS (Pmode, base, new);
		}

	      if (GET_CODE (new) == CONST)
		new = XEXP (new, 0);
              new = force_operand (new, 0);
	    }
	}
    }
  return new;
}

/* Emit insns to move operands[1] into operands[0].  */

void
emit_pic_move (operands, mode)
     rtx *operands;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1]))
    operands[1] = force_reg (Pmode, operands[1]);
  else
    operands[1] = legitimize_pic_address (operands[1], temp);
}

/* Try machine-dependent ways of modifying an illegitimate address X
   to be legitimate.  If we find one, return the new, valid address.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE is the mode of the operand pointed to by X.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address for details.  */

rtx
legitimize_address (x, oldx, mode)
     register rtx x;
     register rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (flag_pic && SYMBOLIC_CONST (x))
    return legitimize_pic_address (x, 0);

  return x;
}


/* Output branch condition code of CODE in assembler
   syntax to stdio stream FILE.  */

static void
output_branch_condition (file, code)
     FILE *file;
     rtx code;
{
  switch (GET_CODE (code)) 
    {
    case EQ:
      fprintf (file, "e");
      break;
    case NE:
      fprintf (file, "ne");
      break;
    case GT:
    case GTU:
      fprintf (file, "h");
      break;
    case LT:
    case LTU:
      fprintf (file, "l");
      break;
    case GE:
    case GEU:
      fprintf (file, "he");
      break;
    case LE:
    case LEU:
      fprintf (file, "le");
      break;
    default:
      fatal_insn ("Unknown CC code", code);
    }
}

/* Output the inverse of the branch condition code of CODE 
   in assembler syntax to stdio stream FILE.  */

static void
output_inverse_branch_condition (file, code)
     FILE *file;
     rtx code;
{
  switch (GET_CODE (code)) 
    {
    case EQ:
      fprintf (file, "ne");
      break;
    case NE:
      fprintf (file, "e");
      break;
    case GT:
    case GTU:
      fprintf (file, "nh");
      break;
    case LT:
    case LTU:
      fprintf (file, "nl");
      break;
    case GE:
    case GEU:
      fprintf (file, "nhe");
      break;
    case LE:
    case LEU:
      fprintf (file, "nle");
      break;
    default:
      fatal_insn ("Unknown CC code", code);
    }
}

/* Output symbolic constant X in assembler syntax to 
   stdio stream FILE.  */

void
s390_output_symbolic_const (file, x)
     FILE *file;
     rtx x;
{
  switch (GET_CODE (x))
    {
    case CONST:
    case ZERO_EXTEND:
    case SIGN_EXTEND:
      s390_output_symbolic_const (file, XEXP (x, 0));
      break;

    case PLUS:
      s390_output_symbolic_const (file, XEXP (x, 0));
      fprintf (file, "+");
      s390_output_symbolic_const (file, XEXP (x, 1));
      break;

    case MINUS:
      s390_output_symbolic_const (file, XEXP (x, 0));
      fprintf (file, "-");
      s390_output_symbolic_const (file, XEXP (x, 1));
      break;

    case CONST_INT:
      output_addr_const (file, x);
      break;

    case LABEL_REF:
    case CODE_LABEL:
      output_addr_const (file, x);
      break;

    case SYMBOL_REF:
      output_addr_const (file, x);
      if (CONSTANT_POOL_ADDRESS_P (x) && s390_pool_count != 0)
        fprintf (file, "_%X", s390_pool_count);
      break;

    case UNSPEC:
      if (XVECLEN (x, 0) != 1)
        output_operand_lossage ("invalid UNSPEC as operand (1)");
      switch (XINT (x, 1))
        {
        case 100:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
          fprintf (file, "-.LT%X_%X", 
                   s390_function_count, s390_pool_count);
	  break;
	case 110:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@GOT12");
	  break;
	case 111:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@GOTENT");
	  break;
	case 112:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@GOT");
	  break;
	case 113:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@PLT");
	  break;
	case 114:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
          fprintf (file, "@PLT-.LT%X_%X",
	           s390_function_count, s390_pool_count);
	  break;
	default:
	  output_operand_lossage ("invalid UNSPEC as operand (2)");
	  break;
        }
      break;

    default:
      fatal_insn ("UNKNOWN in s390_output_symbolic_const !?", x);
      break;
    }
}

/* Output address operand ADDR in assembler syntax to 
   stdio stream FILE.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  struct s390_address ad;

  if (!s390_decompose_address (addr, &ad, TRUE))
    output_operand_lossage ("Cannot decompose address.\n");
 
  if (ad.disp)
    s390_output_symbolic_const (file, ad.disp);
  else
    fprintf (file, "0");

  if (ad.base && ad.indx)
    fprintf (file, "(%s,%s)", reg_names[REGNO (ad.indx)],
                              reg_names[REGNO (ad.base)]);
  else if (ad.base)
    fprintf (file, "(%s)", reg_names[REGNO (ad.base)]);
}

/* Output operand X in assembler syntax to stdio stream FILE.  
   CODE specified the format flag.  The following format flags 
   are recognized:

    'C': print opcode suffix for branch condition.
    'D': print opcode suffix for inverse branch condition.
    'Y': print current constant pool address (pc-relative).
    'y': print current constant pool address (absolute).
    'O': print only the displacement of a memory reference.
    'R': print only the base register of a memory reference.
    'N': print the second word of a DImode operand.
    'M': print the second word of a TImode operand.

    'b': print integer X as if it's a unsigned byte.
    'x': print integer X as if it's a unsigned word.
    'h': print integer X as if it's a signed word.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
    case 'C':
      output_branch_condition (file, x);
      return;

    case 'D':
      output_inverse_branch_condition (file, x);
      return;

    case 'Y':
      fprintf (file, ".LT%X_%X-.", s390_function_count, s390_pool_count);
      return;

    case 'y':
      fprintf (file, ".LT%X_%X", s390_function_count, s390_pool_count);
      return;

    case 'O':
      {
        struct s390_address ad;

        if (GET_CODE (x) != MEM
            || !s390_decompose_address (XEXP (x, 0), &ad, TRUE)
            || ad.indx)
          abort ();

        if (ad.disp)
          s390_output_symbolic_const (file, ad.disp);
        else
          fprintf (file, "0");
      }
      return;

    case 'R':
      {
        struct s390_address ad;

        if (GET_CODE (x) != MEM
            || !s390_decompose_address (XEXP (x, 0), &ad, TRUE)
            || ad.indx)
          abort ();

        if (ad.base)
          fprintf (file, "%s", reg_names[REGNO (ad.base)]);
        else
          fprintf (file, "0");
      }
      return;

    case 'N':
      if (GET_CODE (x) == REG)
	x = gen_rtx_REG (GET_MODE (x), REGNO (x) + 1);
      else if (GET_CODE (x) == MEM)
	x = change_address (x, VOIDmode, plus_constant (XEXP (x, 0), 4));
      else
        abort ();
      break;

    case 'M':
      if (GET_CODE (x) == REG)
	x = gen_rtx_REG (GET_MODE (x), REGNO (x) + 1);
      else if (GET_CODE (x) == MEM)
	x = change_address (x, VOIDmode, plus_constant (XEXP (x, 0), 8));
      else
        abort ();
      break;
    }

  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (x)]);
      break;

    case MEM:
      output_address (XEXP (x, 0));
      break;

    case CONST:
    case CODE_LABEL:
    case LABEL_REF:
    case SYMBOL_REF:
      s390_output_symbolic_const (file, x);
      break;

    case CONST_INT:
      if (code == 'b')
        fprintf (file, "%d", (int)(INTVAL (x) & 0xff));
      else if (code == 'X')
        fprintf (file, "%d", (int)(INTVAL (x) & 0xff));
      else if (code == 'x')
        fprintf (file, "0x%x", (int)(INTVAL (x) & 0xffff));
      else if (code == 'h')
        fprintf (file, "%d", (int)(INTVAL (x) << 16) >> 16);
      else
        fprintf (file, "%d", (int)INTVAL (x));
      break;

    default:
      fatal_insn ("UNKNOWN in print_operand !?", x);
      break;
    }
}

#define DEBUG_SCHED 0

/* Returns true if register REGNO is used  for forming 
   a memory address in expression X.  */

static int
reg_used_in_mem_p (regno, x)
     int regno;
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;
  
  if (code == MEM)
    {
      if (refers_to_regno_p (regno, regno+1,
			     XEXP (x, 0), 0))
	return 1;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e'
	  && reg_used_in_mem_p (regno, XEXP (x, i)))
	return 1;
      
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (reg_used_in_mem_p (regno, XVECEXP (x, i, j)))
	    return 1;
    }
  return 0;
}

/* Returns true if expression DEP_RTX sets a address register
   used by instruction INSN to address memory.  */

static int 
addr_generation_dependency_p (dep_rtx, insn)
     rtx dep_rtx; 
     rtx insn;
{
  rtx target;

  if (GET_CODE (dep_rtx) == SET)
    {
      target = SET_DEST (dep_rtx);
      
      if (GET_CODE (target) == REG)
	{
	  int regno = REGNO (target);

	  if (get_attr_type (insn) == TYPE_LA)
	    return refers_to_regno_p (regno, regno+1,
				      SET_SRC (PATTERN (insn)), 0);
	  else if (get_attr_atype (insn) == ATYPE_MEM)
	    return reg_used_in_mem_p (regno, PATTERN (insn));
	}
    }
  return 0;
}


/* Return the modified cost of the dependency of instruction INSN
   on instruction DEP_INSN through the link LINK.  COST is the 
   default cost of that dependency.

   Data dependencies are all handled without delay.  However, if a
   register is modified and subsequently used as base or index 
   register of a memory reference, at least 4 cycles need to pass
   between setting and using the register to avoid pipeline stalls.  */

int
s390_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  rtx dep_rtx;
  int i;

  /* If the dependence is an anti-dependence, there is no cost.  For an
     output dependence, there is sometimes a cost, but it doesn't seem
     worth handling those few cases.  */

  if (REG_NOTE_KIND (link) != 0)
    return 0;

  /* If we can't recognize the insns, we can't really do anything.  */
  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  /* If cost equal 1 nothing needs to be checked. */

  if (cost == 1)
    {
      return cost;
    }

  dep_rtx = PATTERN (dep_insn);

  if (GET_CODE (dep_rtx) == SET)
    {
      if (addr_generation_dependency_p (dep_rtx, insn))
	{
	  if (DEBUG_SCHED)
	    {
	      fprintf (stderr, "\n\nAddress dependency detected: cost %d\n",
		       cost);
	      debug_rtx (dep_insn);
	      debug_rtx (insn);
	    }
	  return cost;
	}
    }

  else if (GET_CODE (dep_rtx) == PARALLEL)
    {
      for (i = 0; i < XVECLEN (dep_rtx, 0); i++)
	{
	  if (addr_generation_dependency_p (XVECEXP (dep_rtx, 0, i),
					    insn))
	    {
	      if (DEBUG_SCHED)
		{
		  fprintf (stderr, "\n\nAddress dependency detected: cost %d\n"
			   ,cost);
		  debug_rtx (dep_insn);
		  debug_rtx (insn);
		}
	      return cost;
	    }
	}
    }

  /* default cost.  */
  return 1;
}

/* Pool concept for Linux 390:
   - Function prologue saves used register 
   - literal pool is dumped in prologue and  jump across with bras
   - If function has more than 4 k literals, at about every 
     S390_CHUNK_MAX offset in the function a literal pool will be
     dumped
     - in this case, a branch from one chunk to other chunk needs
       a reload of base register at the code label branched to.  */

/* Index of constant pool chunk that is currently being processed.
   Set to -1 before function output has started.  */
int s390_pool_count = -1;

/* First insn using the constant pool chunk that is currently being
   processed.  */
rtx s390_pool_start_insn = NULL_RTX;

/* UID of last insn using the constant pool chunk that is currently 
   being processed.  */
static int pool_stop_uid;

/* Called from the ASM_OUTPUT_POOL_PROLOGUE macro to 
   prepare for printing a literal pool chunk to stdio stream FILE.  

   FNAME and FNDECL specify the name and type of the current function.
   SIZE is the size in bytes of the current literal pool.  */
 
void 
s390_asm_output_pool_prologue (file, fname, fndecl, size)
     FILE *file;
     const char *fname ATTRIBUTE_UNUSED;
     tree fndecl;
     int size ATTRIBUTE_UNUSED;
{

  if (s390_pool_count>0) {
    /*
     * We are in an internal pool, branch over
     */
    if (TARGET_64BIT)
      {
	fprintf (file, "\tlarl\t%s,.LT%X_%X\n", 
		 reg_names[BASE_REGISTER],
		 s390_function_count, s390_pool_count);
	readonly_data_section ();
	ASM_OUTPUT_ALIGN (file, floor_log2 (3));
	fprintf (file, ".LT%X_%X:\t# Pool %d\n",
		 s390_function_count, s390_pool_count, s390_pool_count);
      }
    else
    fprintf (file,"\t.align 4\n\tbras\t%s,0f\n.LT%X_%X:\t# Pool %d \n",
	     reg_names[BASE_REGISTER],
	     s390_function_count, s390_pool_count, s390_pool_count);
  }
  if (!TARGET_64BIT)
    function_section (fndecl);
}

/* Return true if OTHER_ADDR is in different chunk than MY_ADDR.
   LTORG points to a list of all literal pools inserted
   into the current function.  */

static int
other_chunk (ltorg, my_addr, other_addr)
     int *ltorg;
     int my_addr;
     int other_addr;
{
  int ad, i=0, j=0;

  while ((ad = ltorg[i++])) {
    if (INSN_ADDRESSES (ad) >= my_addr)
      break;
  }

  while ((ad = ltorg[j++])) {
    if (INSN_ADDRESSES (ad) > other_addr)
      break;
  }
  
  if (i==j)
    return 0;

  return 1;
}

/* Return true if OTHER_ADDR is too far away from MY_ADDR
   to use a relative branch instruction.  */

static int 
far_away (my_addr, other_addr)
     int my_addr;
     int other_addr;
{
  /* In 64 bit mode we can jump +- 4GB.  */
  if (TARGET_64BIT)
    return 0;
  if (abs (my_addr - other_addr) > S390_REL_MAX)
    return 1;
  return 0;
}

/* Go through all insns in the current function (starting
   at INSN), replacing branch insn if necessary.  A branch
   needs to be modified if either the distance to the 
   target is too far to use a relative branch, or if the
   target uses a different literal pool than the origin.
   LTORG_UIDS points to a list of all literal pool insns
   that have been inserted.  */

static rtx 
check_and_change_labels (insn, ltorg_uids)
     rtx insn;
     int *ltorg_uids;
{
  rtx temp_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);
  rtx target, jump;
  rtx pattern, tmp, body, label1;
  int addr0, addr1;

  if (GET_CODE (insn) != JUMP_INSN) 
    return insn;

  pattern = PATTERN (insn);
  
  addr0 = INSN_ADDRESSES (INSN_UID (insn));
  if (GET_CODE (pattern) == SET) 
    {
      body = XEXP (pattern, 1);
      if (GET_CODE (body) == LABEL_REF) 
	{
	  addr1 = INSN_ADDRESSES (INSN_UID (XEXP (body, 0)));
	  
	  if (other_chunk (ltorg_uids, addr0, addr1)) 
	    {
	      SYMBOL_REF_USED (XEXP (body, 0)) = 1;
	    } 
	  if (far_away (addr0, addr1)) 
	    {
	      if (flag_pic) 
		{
		  target = gen_rtx_UNSPEC (SImode, gen_rtvec (1, body), 100);
		  target = gen_rtx_CONST (SImode, target);
		  target = force_const_mem (SImode, target);
		  jump = gen_rtx_REG (Pmode, BASE_REGISTER);
		  jump = gen_rtx_PLUS (Pmode, jump, temp_reg);
		} 
	      else 
		{
		  target = force_const_mem (Pmode, body);
		  jump = temp_reg;
		}
	      
	      emit_insn_before (gen_movsi (temp_reg, target), insn);
	      tmp = emit_jump_insn_before (gen_indirect_jump (jump), insn);
	      remove_insn (insn);
	      INSN_ADDRESSES_NEW (tmp, -1);
	      return tmp;
	    }
	} 
      else if (GET_CODE (body) == IF_THEN_ELSE) 
	{
	  if (GET_CODE (XEXP (body, 1)) == LABEL_REF) 
	    {
	      addr1 = INSN_ADDRESSES (INSN_UID (XEXP (XEXP (body, 1), 0)));
	      
	      if (other_chunk (ltorg_uids, addr0, addr1)) 
		{
		  SYMBOL_REF_USED (XEXP (XEXP (body, 1), 0)) = 1;
		} 
	      
	      if (far_away (addr0, addr1)) 
		{
		  if (flag_pic) 
		    {
		      target = gen_rtx_UNSPEC (SImode, gen_rtvec (1, XEXP (body, 1)), 100);
		      target = gen_rtx_CONST (SImode, target);
		      target = force_const_mem (SImode, target);
		      jump = gen_rtx_REG (Pmode, BASE_REGISTER);
		      jump = gen_rtx_PLUS (Pmode, jump, temp_reg);
		    } 
		  else 
		    {
		      target = force_const_mem (Pmode, XEXP (body, 1));
		      jump = temp_reg;
		    }
		  
		  label1 = gen_label_rtx ();
		  emit_jump_insn_before (gen_icjump (label1, XEXP (body, 0)), insn);
		  emit_insn_before (gen_movsi (temp_reg, target), insn);
		  tmp = emit_jump_insn_before (gen_indirect_jump (jump), insn);
		  INSN_ADDRESSES_NEW (emit_label_before (label1, insn), -1);
		  remove_insn (insn);
		  return tmp;
		}
	    }
	  else if (GET_CODE (XEXP (body, 2)) == LABEL_REF) 
	    {
	      addr1 = INSN_ADDRESSES (INSN_UID (XEXP (XEXP (body, 2), 0)));
	      
	      if (other_chunk (ltorg_uids, addr0, addr1)) 
		{
		  SYMBOL_REF_USED (XEXP (XEXP (body, 2), 0)) = 1;
		} 
	      
	      if (far_away (addr0, addr1)) 
		{
		  if (flag_pic) 
		    {
		      target = gen_rtx_UNSPEC (SImode, gen_rtvec (1, XEXP (body, 2)), 100);
		      target = gen_rtx_CONST (SImode, target);
		      target = force_const_mem (SImode, target);
		      jump = gen_rtx_REG (Pmode, BASE_REGISTER);
		      jump = gen_rtx_PLUS (Pmode, jump, temp_reg);
		    } 
		  else 
		    {
		      target = force_const_mem (Pmode, XEXP (body, 2));
		      jump = temp_reg;
		    }
		  
		  label1 = gen_label_rtx ();
		  emit_jump_insn_before (gen_cjump (label1, XEXP (body, 0)), insn);
		  emit_insn_before (gen_movsi (temp_reg, target), insn);
		  tmp = emit_jump_insn_before (gen_indirect_jump (jump), insn);
		  INSN_ADDRESSES_NEW (emit_label_before (label1, insn), -1);
		  remove_insn (insn);
		  return tmp;
		}
	    }
	}
    } 
  else if (GET_CODE (pattern) == ADDR_VEC || 
	   GET_CODE (pattern) == ADDR_DIFF_VEC) 
    {
      int i, diff_vec_p = GET_CODE (pattern) == ADDR_DIFF_VEC;
      int len = XVECLEN (pattern, diff_vec_p);
      
      for (i = 0; i < len; i++) 
	{
	  addr1 = INSN_ADDRESSES (INSN_UID (XEXP (XVECEXP (pattern, diff_vec_p, i), 0)));
	  if (other_chunk (ltorg_uids, addr0, addr1)) 
	    {
	      SYMBOL_REF_USED (XEXP (XVECEXP (pattern, diff_vec_p, i), 0)) = 1;
	    } 
	}
    }
  return insn;
}

/* Called from s390_function_prologue to make final adjustments
   before outputting code.  CHUNKIFY specifies whether we need
   to use multiple literal pools (because the total size of the
   literals exceeds 4K).  */

static void
s390_final_chunkify (chunkify)
     int chunkify;
{
  rtx insn, ninsn, tmp;
  int addr, naddr = 0, uids;
  int chunk_max = 0;

  const char *asms;

  int size = insn_current_address;

  int *ltorg_uids;
  int max_ltorg=0;

  ltorg_uids = alloca (size / 1024 + 1024);
  memset (ltorg_uids, 0, size / 1024 + 1024);

  if (chunkify == 1) 
    {
      chunk_max = size * 2048 / get_pool_size ();
      chunk_max = chunk_max > S390_CHUNK_MAX 
	? S390_CHUNK_MAX : chunk_max;
    } 
  
  for (insn=get_insns (); insn;insn = next_real_insn (insn)) 
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;
      
      addr = INSN_ADDRESSES (INSN_UID (insn));
      if ((ninsn = next_real_insn (insn))) 
	{
	  naddr = INSN_ADDRESSES (INSN_UID (ninsn));
	}
      
      if (chunkify && (addr / chunk_max != naddr / chunk_max)) 
	{
	  for (tmp = insn; tmp; tmp = NEXT_INSN (tmp)) 
	    {
	      if (GET_CODE (tmp) == CODE_LABEL && 
		  GET_CODE (NEXT_INSN (tmp)) != JUMP_INSN) 
		{
		  ltorg_uids[max_ltorg++] = INSN_UID (prev_real_insn (tmp));
		  break;
		} 
	      if (GET_CODE (tmp) == CALL_INSN) 
		{
		  ltorg_uids[max_ltorg++] = INSN_UID (tmp);
		  break;
		} 
	      if (INSN_ADDRESSES (INSN_UID (tmp)) - naddr > S390_CHUNK_OV) 
		{
		  debug_rtx (insn);
		  debug_rtx (tmp);
		  fprintf (stderr, "s390 multiple literalpool support:"
			   "\n No code label between this insn %X %X",
			   naddr, INSN_ADDRESSES (INSN_UID (tmp)));
		  abort ();
		}
	    }
	  if (tmp == NULL) 
	    {
	      warning ("no code label found");
	    }
	} 
      else if (GET_CODE (PATTERN (insn)) == ASM_INPUT && !TARGET_64BIT) 
	{
	  asms = XSTR (PATTERN (insn),0);
	  
	  if ((memcmp (asms,".section",8) == 0) ||
	      (memcmp (asms,".text",5) == 0)    ||
	      (memcmp (asms,"\t.section",9) == 0) ||
	      (memcmp (asms,"\t.text",6) == 0))  {
	    ltorg_uids[max_ltorg++] = INSN_UID (insn);
	    INSN_ADDRESSES_NEW (emit_insn_before (gen_rtx_ASM_INPUT (VOIDmode,
					   ".align 4"), insn), -1);
	  }
	}
    }
  ltorg_uids[max_ltorg] = 0;
  for (insn=get_insns (),uids=0; insn;insn = next_real_insn (insn)) 
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;
      if (INSN_UID (insn) == ltorg_uids[uids]) 
	{
	  INSN_ADDRESSES_NEW (emit_insn_after (gen_ltorg (
			      gen_rtx_CONST_INT (Pmode, ltorg_uids[++uids])),
					       insn), -1);
	} 
      if (GET_CODE (insn) == JUMP_INSN) 
	{
	  insn = check_and_change_labels (insn, ltorg_uids);
	}
    }
  if (chunkify) 
    {
    for (insn=get_insns (); insn;insn = next_insn (insn)) 
      {
      if (GET_CODE (insn) == CODE_LABEL) 
	{
	if (SYMBOL_REF_USED (insn)) 
	  {
	    INSN_ADDRESSES_NEW (emit_insn_after (gen_reload_base (
								  gen_rtx_LABEL_REF (Pmode, XEXP (insn, 0))), insn), -1);
	  }
	}
      }
    }
  pool_stop_uid = ltorg_uids[0];
}

/* Return true if INSN is a 'ltorg' insn.  */

int 
s390_stop_dump_lit_p (insn)
    rtx insn;
{
  rtx body=PATTERN (insn);
  if (GET_CODE (body) == PARALLEL
      && GET_CODE (XVECEXP (body, 0, 0)) == SET
      && GET_CODE (XVECEXP (body, 0, 1)) == USE
      && GET_CODE (XEXP ((XVECEXP (body, 0, 1)),0)) == CONST_INT
      && GET_CODE (SET_DEST (XVECEXP (body, 0, 0))) == REG
      && REGNO (SET_DEST (XVECEXP (body, 0, 0))) == BASE_REGISTER
      && SET_SRC (XVECEXP (body, 0, 0)) == pc_rtx) {
    return 1;
  }
  else
    return 0;   
}

/* Output literal pool chunk to be used for insns
   between insn ACT_INSN and the insn with UID STOP.  */

void
s390_dump_literal_pool (act_insn, stop)
     rtx act_insn;
     rtx stop;
{
  s390_pool_start_insn = act_insn;
  pool_stop_uid = INTVAL (stop);
  s390_pool_count++;
  output_constant_pool (current_function_name, current_function_decl);
  function_section (current_function_decl);
}


#ifdef DWARF2_DEBUGGING_INFO
extern char *dwarf2out_cfi_label PARAMS ((void));
#endif

/* Flag set in prologue, used in epilog to know
   if stack is allocated or not.  */
static int leaf_function_flag;

/* Symbol references needed by the profile code;
   set up by the function prologue routine if necessary.  */
rtx s390_profile[10];

/* Number of elements of current constant pool.  */
int s390_nr_constants;

/* Return true if floating point registers need to be saved.  */

static int 
save_fprs_p ()
{
  int i;
  if (!TARGET_64BIT)
    return 0;
  for (i=24; i<=31; i++) 
    {
      if (regs_ever_live[i] == 1)
	return 1;
    }
  return 0;
}

/* Return true if urrent function is a leaf function, 
   without automatics, alloca or vararg stuff.  */

static int
cur_is_leaf_function ()
{
  int lsize =  get_frame_size () + current_function_outgoing_args_size
    + save_fprs_p () * 64;

  if (leaf_function_p () && ((lsize) == 0) &&
      ! (current_function_calls_alloca) &&
      ! (current_function_stdarg) && ! (current_function_varargs))
    return 1;
  return 0;
}

/* Return offset between argument pointer and frame pointer 
   initially after prologue.  */

int 
s390_arg_frame_offset ()
{
  int lsize =  get_frame_size () + current_function_outgoing_args_size
    + save_fprs_p () * 64;

  if (cur_is_leaf_function ())
    return STACK_POINTER_OFFSET;
  else
    return 2*STACK_POINTER_OFFSET + lsize;
}

/* Output code to stdio stream FILE to save floating point 
   registers on current stack, at offset OFFSET to the frame
   pointer register FP.  */

static int 
save_fprs (file, offset, fp)
     FILE *file;
     long offset;
     int fp;
{
  int i;

  if (!TARGET_64BIT)
    return 0;

  for (i=24; i<=31; i++) 
    {
      if (regs_ever_live[i] == 1)
	{
	  fprintf (file, "\tstd\t%s,%ld(%s)\n", reg_names[i], 
		   (i-24) * 8 + offset, reg_names[fp]); 
	}
    }

  return 1;
}

/* Output code to stdio stream FILE to restore floating point 
   registers from current stack, at offset OFFSET to the frame
   pointer register FP.  */

static int 
restore_fprs (file, offset, fp)
     FILE *file;
     long offset;
     int fp;
{
  int i;

  if (!TARGET_64BIT)
    return 0;

  if (!save_fprs_p ())
    return 0;

  if (offset < 0) 
    {
      fp = 1;
      offset = 0;
      fprintf (file, "\tlgr\t%s,%s\n", reg_names[fp], 
	       reg_names[STACK_POINTER_REGNUM]); 
      fprintf (file, "\taghi\t%s,-64\n", reg_names[fp]); 
    }

  for (i=24; i<=31; i++) 
    {
      if (regs_ever_live[i] == 1)
	{
	  fprintf (file, "\tld\t%s,%ld(%s)\n", reg_names[i], 
		   (i-24) * 8 + offset, reg_names[fp]); 
	}
    }

  return 1;
}

/* Output main constant pool to stdio stream FILE.  */ 

static void
s390_output_constant_pool (file)
     FILE *file;
{
  /* Output constant pool.  */
  if (s390_nr_constants || regs_ever_live[BASE_REGISTER])
    {
      s390_pool_count = 0;
      if (TARGET_64BIT)
	{
	  fprintf (file, "\tlarl\t%s,.LT%X_%X\n", reg_names[BASE_REGISTER],
		   s390_function_count, s390_pool_count);
	  readonly_data_section ();
	  ASM_OUTPUT_ALIGN (file, floor_log2 (3));
	}
      else
	{
	  fprintf (file, "\tbras\t%s,.LTN%X_%X\n", reg_names[BASE_REGISTER],
		   s390_function_count, s390_pool_count);
	}
      fprintf (file, ".LT%X_%X:\n", s390_function_count, s390_pool_count);
      output_constant_pool (current_function_name, current_function_decl);
      fprintf (file, ".LTN%X_%X:\n", s390_function_count,
	       s390_pool_count);
      if (TARGET_64BIT)
	function_section (current_function_decl);
      
      regs_ever_live[BASE_REGISTER] = 1;
    }
}

/* Add constant CTX to the constant pool at a late time 
   (after the initial pass to count the number of constants
   was already done).  Returns the resulting constant 
   pool reference.  */

static rtx
s390_force_const_mem_late (cst)
     rtx cst;
{
  cst = force_const_mem (Pmode, cst);

  s390_nr_constants++;
  regs_ever_live[BASE_REGISTER] = 1;

  emit_insn_before (gen_rtx (USE, Pmode, cst), get_insns ());

  return cst;
}

/* Add a reference to the symbol NAME to the constant pool.
   FUNC specifies whether NAME refers to a function, while
   GLOBAL specifies whether NAME is a global symbol.  Depending
   on these flags, the appopriate PLT or GOT references are
   generated.  Returns the constant pool reference.  */

static rtx
s390_force_const_mem_symbol (name, func, global)
     const char *name;
     int func;
     int global;
{
  rtx symbol;

  if (TARGET_64BIT)
    abort ();

  symbol = gen_rtx (SYMBOL_REF, Pmode, name);
  SYMBOL_REF_FLAG (symbol) = !global;

  if (flag_pic)
    {
      if (global)
        {
          current_function_uses_pic_offset_table = 1;
          symbol = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, symbol), func? 114 : 112);
          symbol = gen_rtx_CONST (VOIDmode, symbol);
        }
      else
        {
          symbol = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, symbol), 100);
          symbol = gen_rtx_CONST (VOIDmode, symbol);
        }
    }

  return s390_force_const_mem_late (symbol);
}

/* Output the function prologue assembly code to the 
   stdio stream FILE.  The local frame size is passed
   in LSIZE.  */

void
s390_function_prologue (file, lsize)
     FILE *file;
     HOST_WIDE_INT lsize;
{
  extern int profile_label_no;
  int i, j;
  long frame_size;
  rtx stack_label = 0, got_label = 0;
  char *l;
  char b64[2] = " ";
  b64[0] = TARGET_64BIT ? 'g' : '\0';

  /* Check for too large size of local variables */

  if (lsize > 0x7fff0000)
    fatal_error ("Total size of local variables exceeds architecture limit.");

  /* Profile code (-p, -a, -ax needs some literals).  */

  if (profile_block_flag && !TARGET_64BIT)
    {
      s390_profile[0] = s390_force_const_mem_symbol ("__bb_init_func", 1, 1);
      s390_profile[1] = s390_force_const_mem_symbol ("__bb_init_trace_func", 1, 1);
      s390_profile[2] = s390_force_const_mem_symbol ("__bb_trace_func", 1, 1);
      s390_profile[3] = s390_force_const_mem_symbol ("__bb_trace_ret", 1, 1);
      s390_profile[5] = s390_force_const_mem_symbol ("__bb", 0, 1);
      s390_profile[6] = s390_force_const_mem_symbol (".LPBX0", 0, 0);
      s390_profile[7] = s390_force_const_mem_symbol (".LPBX2", 0, 0);
    }

  if (profile_flag && !TARGET_64BIT)
    {
      static char label[128];
      sprintf (label, "%sP%d", LPREFIX, profile_label_no);

      s390_profile[4] = s390_force_const_mem_symbol ("_mcount", 1, 1);
      s390_profile[9] = s390_force_const_mem_symbol (label, 0, 0);
    }

  if (get_pool_size () > S390_POOL_MAX)
    s390_final_chunkify (1);
  else
    s390_final_chunkify (0);

  if (current_function_uses_pic_offset_table)
    regs_ever_live[12] = 1;

  if (!TARGET_64BIT && current_function_uses_pic_offset_table)
    {
      got_label = s390_force_const_mem_symbol ("_GLOBAL_OFFSET_TABLE_", 0, 0);
    }

  if ((frame_size = 
       STARTING_FRAME_OFFSET + lsize + save_fprs_p () * 64) > 0x7fff)
    {
      stack_label = s390_force_const_mem_late (GEN_INT (frame_size));
    }

  if (!optimize)
    {
      /* Stupid register allocation is stupid ...
         It does not always recognize the base register is used. */
      
      regs_ever_live[BASE_REGISTER] = 1;
    }

 if (cur_is_leaf_function ())
   {
      leaf_function_flag = 1;
      fprintf (file, "%s\tleaf function\n", ASM_COMMENT_START);
      fprintf (file, "%s\thas varargs             %d\n", ASM_COMMENT_START,
	       current_function_stdarg);
      fprintf (file, "%s\tincoming args (stack)   %d\n", ASM_COMMENT_START,
	       current_function_args_size);
      fprintf (file, "%s\tfunction length         %d\n", ASM_COMMENT_START,
	       insn_current_address);
      fprintf (file, "%s\tregister live           ", ASM_COMMENT_START);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	fprintf (file, "%d", regs_ever_live[i]);
      fputc   ('\n',file);
      
      /* Save gprs 6 - 15 and fprs 4 and 6.  */
      for (i = 6; i < 13 && (regs_ever_live[i] == 0); i++);

      if (s390_nr_constants || regs_ever_live[13] || i != 13)
	{
	  fprintf (file, "\tstm%s\t%s,%s,%d(%s)\n", 
                         b64, reg_names[i], reg_names[13],
                         i * UNITS_PER_WORD,
		         reg_names[STACK_POINTER_REGNUM]);
#ifdef INCOMING_RETURN_ADDR_RTX
	  if (dwarf2out_do_frame ())
	    {
	      l = dwarf2out_cfi_label ();
	      dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, 
				 STACK_POINTER_OFFSET);
	      for (j = i; j <= 14; j++)
		dwarf2out_reg_save (l, j, (TARGET_64BIT ? (j-20) : (j-24))
				    * UNITS_PER_WORD);
	      if (regs_ever_live[18])
		dwarf2out_reg_save (l, 18, -16);
	      if (regs_ever_live[19])
		dwarf2out_reg_save (l, 19, -8);
	    }
#endif
	}

      s390_output_constant_pool (file);

      /* Save fprs.  */

      if (!TARGET_64BIT)
	{
	  if (regs_ever_live[18])
	    fprintf (file, "\tstd\t4,80(%s)\n", reg_names[STACK_POINTER_REGNUM]);
	  if (regs_ever_live[19])
	    fprintf (file, "\tstd\t6,88(%s)\n", reg_names[STACK_POINTER_REGNUM]);
	}
    }
  else
    {				/* No leaf function.  */
      fprintf (file, "%s\tleaf function           %d\n", ASM_COMMENT_START,
	       leaf_function_p ());
      fprintf (file, "%s\tautomatics              %d\n", ASM_COMMENT_START,
	       (int)lsize);
      fprintf (file, "%s\toutgoing args           %d\n", ASM_COMMENT_START,
	       current_function_outgoing_args_size);
      fprintf (file, "%s\tneed frame pointer      %d\n", ASM_COMMENT_START,
	       frame_pointer_needed);
      fprintf (file, "%s\tcall alloca             %d\n", ASM_COMMENT_START,
	       current_function_calls_alloca);
      fprintf (file, "%s\thas varargs             %d\n", ASM_COMMENT_START,
	       current_function_stdarg || current_function_varargs);
      fprintf (file, "%s\tincoming args (stack)   %d\n", ASM_COMMENT_START,
	       current_function_args_size);
      fprintf (file, "%s\tfunction length         %d\n", ASM_COMMENT_START,
	       insn_current_address);
      fprintf (file, "%s\tregister live           ", ASM_COMMENT_START);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	fprintf (file, "%d", regs_ever_live[i]);
      fputc   ('\n',file);

      /* Save gprs 6 - 15 and fprs 4 and 6.  */
      
      if (current_function_stdarg || current_function_varargs)
	{
	  i = 2;
	}
      else
	{
	  for (i = 6; i < 13 && (regs_ever_live[i] == 0); i++);
	}

      fprintf (file, "\tstm%s\t%s,%s,%d(%s)\n", 
                     b64, reg_names[i], reg_names[15], i * UNITS_PER_WORD,
	             reg_names[STACK_POINTER_REGNUM]);

#ifdef INCOMING_RETURN_ADDR_RTX
      if (dwarf2out_do_frame ())
	{
	  l = dwarf2out_cfi_label ();
	  dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, STACK_POINTER_OFFSET);
	  for (j = i; j <= 15; j++)
	    dwarf2out_reg_save (l, j, (TARGET_64BIT ? (j-20) : (j-24)) *
				UNITS_PER_WORD);
	  if (regs_ever_live[18])
	    dwarf2out_reg_save (l, 18, -16);
	  if (regs_ever_live[19])
	    dwarf2out_reg_save (l, 19, -8);
	}
#endif

      s390_output_constant_pool (file);

      /* Save fprs.  */

      if (current_function_stdarg || current_function_varargs)
	{
	  fprintf (file, "\tstd\t%s,%d(%s)\n", 
		   reg_names[16],
		   STACK_POINTER_OFFSET-32,
		   reg_names[STACK_POINTER_REGNUM]);
	  fprintf (file, "\tstd\t%s,%d(%s)\n",
		   reg_names[17],
		   STACK_POINTER_OFFSET-24,
		   reg_names[STACK_POINTER_REGNUM]);
	  if (TARGET_64BIT)
	    {
	      fprintf (file, "\tstd\t%s,%d(%s)\n", 
		       reg_names[18],
		       STACK_POINTER_OFFSET-16,
		       reg_names[STACK_POINTER_REGNUM]);
	      fprintf (file, "\tstd\t%s,%d(%s)\n", 
		       reg_names[19],
		       STACK_POINTER_OFFSET-8,
		       reg_names[STACK_POINTER_REGNUM]);
	    }
	}
      if (!TARGET_64BIT)
	{
	  if (regs_ever_live[18])
	    fprintf (file, "\tstd\t%s,%d(%s)\n", 
		     reg_names[18],
		     STACK_POINTER_OFFSET-16,
		     reg_names[STACK_POINTER_REGNUM]);
	  if (regs_ever_live[19])
	    fprintf (file, "\tstd\t%s,%d(%s)\n",
		     reg_names[19],
		     STACK_POINTER_OFFSET-8,
		     reg_names[STACK_POINTER_REGNUM]);
	}

      
      if (save_fprs_p () && frame_size > 4095) 
	{
	  int fp = 1;
	  fprintf (file, "\tlgr\t%s,%s\n", reg_names[fp], 
		   reg_names[STACK_POINTER_REGNUM]); 
	  fprintf (file, "\taghi\t%s,-64\n", reg_names[fp]);
	  save_fprs (file, 0, fp);
	}

      /* Decrement stack.  */

      if (TARGET_BACKCHAIN || (STARTING_FRAME_OFFSET +
			       lsize + STACK_POINTER_OFFSET > 4095
			       || frame_pointer_needed
			       || current_function_calls_alloca))
	{

	  fprintf (file, "\tl%sr\t%s,%s\n", b64, 
			 reg_names[1], reg_names[STACK_POINTER_REGNUM]);
	}

      if (stack_label)
	{
	  rtx operands[2];

	  operands[0] = stack_pointer_rtx;
	  operands[1] = stack_label;
	  if (TARGET_64BIT)
	    output_asm_insn ("sg\t%0,%1", operands);
	  else
	    output_asm_insn ("s\t%0,%1", operands);
	}
      else
	{
	  fprintf (file, "\ta%shi\t%s,-%ld\n",b64, 
		   reg_names[STACK_POINTER_REGNUM], frame_size);
	}
#ifdef INCOMING_RETURN_ADDR_RTX
      if (dwarf2out_do_frame ())
	{
	  if (frame_pointer_needed)
	    dwarf2out_def_cfa ("", HARD_FRAME_POINTER_REGNUM,
			       STACK_POINTER_OFFSET+frame_size);
	  else
	    dwarf2out_def_cfa ("", STACK_POINTER_REGNUM,
			       STACK_POINTER_OFFSET+frame_size);
	}
#endif


      /* Generate backchain.  */

      if (TARGET_BACKCHAIN || (STARTING_FRAME_OFFSET + 
			       lsize + STACK_POINTER_OFFSET > 4095
			       || frame_pointer_needed
			       || current_function_calls_alloca))
	{
	  fprintf (file, "\tst%s\t%s,0(%s)\n", 
                         b64, reg_names[1], reg_names[STACK_POINTER_REGNUM]);
	}
    }

  if (frame_pointer_needed)
    {
      fprintf (file, "\tl%sr\t%s,%s\n", b64, 
                     reg_names[FRAME_POINTER_REGNUM], 
                     reg_names[STACK_POINTER_REGNUM]);
    }

  /* Load GOT if used and emit use insn that optimizer does not
     erase literal pool entry.  */

  if (current_function_uses_pic_offset_table)
    {
      rtx operands[3];
      if (TARGET_64BIT)
	{
	  fprintf (file, "\tlarl\t%s,_GLOBAL_OFFSET_TABLE_\n",
			 reg_names[PIC_OFFSET_TABLE_REGNUM]);
	}
      else
	{
	  operands[0] = gen_rtx (REG, Pmode, PIC_OFFSET_TABLE_REGNUM);
	  operands[1] = got_label;
	  operands[2] = gen_rtx (REG, Pmode, BASE_REGISTER);
	  output_asm_insn ("l\t%0,%1\n\tar\t%0,%2", operands);
	}
    }
  /* Save FPRs below save area.  */

  if (frame_size <= 4095)
    save_fprs (file, frame_size - 64, STACK_POINTER_REGNUM);

  return;
}

/* Output the function epilogue assembly code to the 
   stdio stream FILE.  The local frame size is passed
   in LSIZE.  */

void
s390_function_epilogue (file, lsize)
     FILE *file;
     HOST_WIDE_INT lsize;
{
/* Register is call clobbered and not used for eh or return.  */
#define FREE_REG 4

  int i;
  long frame_size;
  int return_reg = RETURN_REGNUM;
  int fp, offset;
  char b64[2] = " ";

  b64[0] = TARGET_64BIT ? 'g' : '\0';
  frame_size = STARTING_FRAME_OFFSET + lsize + save_fprs_p () * 64;
  
  if (current_function_uses_pic_offset_table)
    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;
  
  if (leaf_function_flag)
    {
      for (i = 6; i < 13 && (regs_ever_live[i] == 0); i++);

      if (s390_nr_constants || regs_ever_live[13] || i != 13)
	{
	    fprintf (file, "\tlm%s\t%s,%s,%d(%s)\n", b64, 
                     reg_names[i], reg_names[13],
		     UNITS_PER_WORD * i,
		     reg_names[STACK_POINTER_REGNUM]);
	}
      if (!TARGET_64BIT)
	{
	  if (regs_ever_live[18])
	    fprintf (file, "\tld\t%s,%d(%s)\n", 
		     reg_names[18],
		     STACK_POINTER_OFFSET-16,
		     reg_names[STACK_POINTER_REGNUM]);
	  if (regs_ever_live[19])
	    fprintf (file, "\tld\t%s,%d(%s)\n",
		     reg_names[19],
		     STACK_POINTER_OFFSET-8,
		     reg_names[STACK_POINTER_REGNUM]);
	}
    }
  else
    {
      for (i = 6; i < 13 && (regs_ever_live[i] == 0); i++);

      if (frame_size + STACK_POINTER_OFFSET > 4095)    
	{
	  offset = 0;
	  fp = STACK_POINTER_REGNUM;
	}
      else if (frame_pointer_needed || current_function_calls_alloca)
	{
	  offset = frame_size;
	  fp = FRAME_POINTER_REGNUM;
	}
      else
	{
	  offset = frame_size;
	  fp = STACK_POINTER_REGNUM;
	}

      /* Restore from offset below save area.  */

      if (offset == 0)
	fprintf (file, "\tl%s\t%s,0(%s)\n", b64, 
		       reg_names[fp], reg_names[fp]);
      restore_fprs (file, offset-64, fp);
      return_reg = FREE_REG;
      fprintf (file, "\tl%s\t%s,%d(%s)\n", b64, reg_names[return_reg], 
	       UNITS_PER_WORD*RETURN_REGNUM+offset, reg_names[fp]);
      if (!TARGET_64BIT)
	{
	  if (regs_ever_live[18])
	    fprintf (file, "\tld\t%s,%d(%s)\n", 
		     reg_names[18],
		     offset+STACK_POINTER_OFFSET-16, reg_names[fp]);
	  if (regs_ever_live[19])
	    fprintf (file, "\tld\t%s,%d(%s)\n",
		     reg_names[19],
		     offset+STACK_POINTER_OFFSET-8, reg_names[fp]);
	}
      fprintf (file, "\tlm%s\t%s,%s,%d(%s)\n", b64, 
		     reg_names[i], reg_names[15],
	             (UNITS_PER_WORD * i) + offset, reg_names[fp]);
    }
  
  fprintf (file, "\tbr\t%s\n", reg_names[return_reg]);

  current_function_uses_pic_offset_table = 0;
  leaf_function_flag = 0;
  s390_pool_start_insn = NULL_RTX;
  s390_pool_count = -1;
  s390_function_count++;
  return;
}


/* Return the size in bytes of a function argument of 
   type TYPE and/or mode MODE.  At least one of TYPE or
   MODE must be specified.  */

static int
s390_function_arg_size (mode, type)
     enum machine_mode mode;
     tree type;
{
  if (type)
    return int_size_in_bytes (type);

  /* No type info available for some library calls ... */
  if (mode != BLKmode)
    return GET_MODE_SIZE (mode);

  /* If we have neither type nor mode, abort */
  abort ();
}

/* Return 1 if a function argument of type TYPE and mode MODE
   is to be passed by reference.  The ABI specifies that only
   structures of size 1, 2, 4, or 8 bytes are passed by value,
   all other structures (and complex numbers) are passed by
   reference.  */

int
s390_function_arg_pass_by_reference (mode, type)
     enum machine_mode mode;
     tree type;
{
  int size = s390_function_arg_size (mode, type);

  if (type)
    {
      if (AGGREGATE_TYPE_P (type) &&
          size != 1 && size != 2 && size != 4 && size != 8)
        return 1;

      if (TREE_CODE (type) == COMPLEX_TYPE)
        return 1;
    }
  return 0;

}

/* Update the data in CUM to advance over an argument of mode MODE and
   data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.).  The boolean NAMED specifies whether the
   argument is a named argument (as opposed to an unnamed argument
   matching an ellipsis).  */

void
s390_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if (! TARGET_SOFT_FLOAT && (mode == DFmode || mode == SFmode))
    {
      cum->fprs++;
    }
  else if (s390_function_arg_pass_by_reference (mode, type))
    {
      cum->gprs += 1;
    }
  else
    {
      int size = s390_function_arg_size (mode, type);
      cum->gprs += ((size + UNITS_PER_WORD-1) / UNITS_PER_WORD);
    }
}

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  

   On S/390, we use general purpose registers 2 through 6 to
   pass integer, pointer, and certain structure arguments, and
   floating point registers 0 and 2 (0, 2, 4, and 6 on 64-bit)
   to pass floating point arguments.  All remaining arguments
   are pushed to the stack.  */

rtx
s390_function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if (s390_function_arg_pass_by_reference (mode, type))
      return 0;

  if (! TARGET_SOFT_FLOAT && (mode == DFmode || mode == SFmode))
    {
      if (cum->fprs + 1 > (TARGET_64BIT? 4 : 2))
	return 0;
      else
	return gen_rtx (REG, mode, cum->fprs + 16);
    }
  else
    {
      int size = s390_function_arg_size (mode, type);
      int n_gprs = (size + UNITS_PER_WORD-1) / UNITS_PER_WORD;

      if (cum->gprs + n_gprs > 5)
	return 0;
      else
	return gen_rtx (REG, mode, cum->gprs + 2);
    }
}


/* Create and return the va_list datatype.

   On S/390, va_list is an array type equivalent to

      typedef struct __va_list_tag
        {
            long __gpr;
            long __fpr;
            void *__overflow_arg_area;
            void *__reg_save_area;
            
        } va_list[1];

   where __gpr and __fpr hold the number of general purpose
   or floating point arguments used up to now, respectively,
   __overflow_arg_area points to the stack location of the 
   next argument passed on the stack, and __reg_save_area
   always points to the start of the register area in the
   call frame of the current function.  The function prologue
   saves all registers used for argument passing into this
   area if the function uses variable arguments.  */

tree
s390_build_va_list ()
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  record = make_lang_type (RECORD_TYPE);

  type_decl =
    build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (FIELD_DECL, get_identifier ("__gpr"), 
		      long_integer_type_node);
  f_fpr = build_decl (FIELD_DECL, get_identifier ("__fpr"), 
		      long_integer_type_node);
  f_ovf = build_decl (FIELD_DECL, get_identifier ("__overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (FIELD_DECL, get_identifier ("__reg_save_area"),
		      ptr_type_node);

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  TREE_CHAIN (f_gpr) = f_fpr;
  TREE_CHAIN (f_fpr) = f_ovf;
  TREE_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start by filling the va_list structure VALIST.
   STDARG_P is true if implementing __builtin_stdarg_va_start,
   false if implementing __builtin_varargs_va_start.  NEXTARG
   points to the first anonymous stack argument.

   The following global variables are used to initalize
   the va_list structure:

     current_function_args_info:
       holds number of gprs and fprs used for named arguments.
     current_function_arg_offset_rtx:
       holds the offset of the first anonymous stack argument
       (relative to the virtual arg pointer).  */

void
s390_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT n_gpr, n_fpr;
  int off;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  /* Count number of gp and fp argument registers used.  */

  n_gpr = current_function_args_info.gprs;
  n_fpr = current_function_args_info.fprs;

  t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr, build_int_2 (n_gpr, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build (MODIFY_EXPR, TREE_TYPE (fpr), fpr, build_int_2 (n_fpr, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the overflow area.  */
  t = make_tree (TREE_TYPE (ovf), virtual_incoming_args_rtx);

  off = INTVAL (current_function_arg_offset_rtx);
  off = off < 0 ? 0 : off;
  if (! stdarg_p)
    off = off > 0 ? off - UNITS_PER_WORD : off;
  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "va_start: n_gpr = %d, n_fpr = %d off %d\n",
	     (int)n_gpr, (int)n_fpr, off);

  t = build (PLUS_EXPR, TREE_TYPE (ovf), t, build_int_2 (off, 0));

  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the register save area.  */
  t = make_tree (TREE_TYPE (sav), virtual_incoming_args_rtx);
  t = build (PLUS_EXPR, TREE_TYPE (sav), t,
	     build_int_2 (-STACK_POINTER_OFFSET, -1));
  t = build (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement va_arg by updating the va_list structure 
   VALIST as required to retrieve an argument of type
   TYPE, and returning that argument. 
   
   Generates code equivalent to:
   
   if (integral value) {
     if (size  <= 4 && args.gpr < 5 ||
         size  > 4 && args.gpr < 4 ) 
       ret = args.reg_save_area[args.gpr+8]
     else
       ret = *args.overflow_arg_area++;
   } else if (float value) {
     if (args.fgpr < 2)
       ret = args.reg_save_area[args.fpr+64]
     else
       ret = *args.overflow_arg_area++;
   } else if (aggregate value) {
     if (args.gpr < 5)
       ret = *args.reg_save_area[args.gpr]
     else
       ret = **args.overflow_arg_area++;
   } */

rtx
s390_va_arg (valist, type)
     tree valist;
     tree type;
{
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, reg, t, u;
  int indirect_p, size, n_reg, sav_ofs, sav_scale, max_reg;
  rtx lab_false, lab_over, addr_rtx, r;

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  size = int_size_in_bytes (type);

  if (s390_function_arg_pass_by_reference (TYPE_MODE (type), type))
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: aggregate type");
	  debug_tree (type);
	}

      /* Aggregates are passed by reference.  */
      indirect_p = 1;
      reg = gpr;
      n_reg = 1;
      sav_ofs = 2 * UNITS_PER_WORD;
      sav_scale = UNITS_PER_WORD;
      size = UNITS_PER_WORD;
      max_reg = 4;
    }
  else if (FLOAT_TYPE_P (type) && ! TARGET_SOFT_FLOAT)
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: float type");
	  debug_tree (type);
	}

      /* FP args go in FP registers, if present.  */
      indirect_p = 0;
      reg = fpr;
      n_reg = 1;
      sav_ofs = 16 * UNITS_PER_WORD;
      sav_scale = 8;
      /* TARGET_64BIT has up to 4 parameter in fprs */
      max_reg = TARGET_64BIT ? 3 : 1;
    }
  else
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: other type");
	  debug_tree (type);
	}

      /* Otherwise into GP registers.  */
      indirect_p = 0;
      reg = gpr;
      n_reg = (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      sav_ofs = 2 * UNITS_PER_WORD;
      if (TARGET_64BIT)
	sav_ofs += TYPE_MODE (type) == SImode ? 4 : 
	           TYPE_MODE (type) == HImode ? 6 : 
	           TYPE_MODE (type) == QImode ? 7 : 0;
      else
	sav_ofs += TYPE_MODE (type) == HImode ? 2 : 
	           TYPE_MODE (type) == QImode ? 3 : 0;

      sav_scale = UNITS_PER_WORD;
      if (n_reg > 1)
	max_reg = 3;
      else
	max_reg = 4;
    }

  /* Pull the value out of the saved registers ...  */

  lab_false = gen_label_rtx ();
  lab_over = gen_label_rtx ();
  addr_rtx = gen_reg_rtx (Pmode);

  emit_cmp_and_jump_insns (expand_expr (reg, NULL_RTX, Pmode, EXPAND_NORMAL),
			   GEN_INT (max_reg),
			   GT, const1_rtx, Pmode, 0, 1, lab_false);

  if (sav_ofs)
    t = build (PLUS_EXPR, ptr_type_node, sav, build_int_2 (sav_ofs, 0));
  else
    t = sav;

  u = build (MULT_EXPR, long_integer_type_node,
	     reg, build_int_2 (sav_scale, 0));
  TREE_SIDE_EFFECTS (u) = 1;

  t = build (PLUS_EXPR, ptr_type_node, t, u);
  TREE_SIDE_EFFECTS (t) = 1;

  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);


  emit_jump_insn (gen_jump (lab_over));
  emit_barrier ();
  emit_label (lab_false);

  /* ... Otherwise out of the overflow area.  */

  t = save_expr (ovf);


  /* In 64 BIT for each argument on stack, a full 64 bit slot is allocated.  */
  if (size < UNITS_PER_WORD)
    {
      t = build (PLUS_EXPR, TREE_TYPE (t), t, build_int_2 (UNITS_PER_WORD-size, 0));
      t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      t = save_expr (ovf);
    }

  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);

  t = build (PLUS_EXPR, TREE_TYPE (t), t, build_int_2 (size, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  emit_label (lab_over);

  /* If less than max_regs a registers are retrieved out 
     of register save area, increment.  */

  u = build (PREINCREMENT_EXPR, TREE_TYPE (reg), reg, 
	     build_int_2 (n_reg, 0));
  TREE_SIDE_EFFECTS (u) = 1;
  expand_expr (u, const0_rtx, VOIDmode, EXPAND_NORMAL);

  if (indirect_p)
    {
      r = gen_rtx_MEM (Pmode, addr_rtx);
      MEM_ALIAS_SET (r) = get_varargs_alias_set ();
      emit_move_insn (addr_rtx, r);
    }


  return addr_rtx;
}


/* Output assembly code for the trampoline template to
   stdio stream FILE.

   On S/390, we use gpr 1 internally in the trampoline code;
   gpr 0 is used to hold the static chain.  */

void
s390_trampoline_template (file)
     FILE *file;
{
  if (TARGET_64BIT)
    {
      fprintf (file, "larl\t%s,0f\n", reg_names[1]);
      fprintf (file, "lg\t%s,0(%s)\n", reg_names[0], reg_names[1]);
      fprintf (file, "lg\t%s,8(%s)\n", reg_names[1], reg_names[1]);
      fprintf (file, "br\t%s\n", reg_names[1]);
      fprintf (file, "0:\t.quad\t0\n");
      fprintf (file, ".quad\t0\n");
    }
  else
    {
      fprintf (file, "basr\t%s,0\n", reg_names[1]);
      fprintf (file, "l\t%s,10(%s)\n", reg_names[0], reg_names[1]);
      fprintf (file, "l\t%s,14(%s)\n", reg_names[1], reg_names[1]);
      fprintf (file, "br\t%s\n", reg_names[1]);
      fprintf (file, ".long\t0\n");
      fprintf (file, ".long\t0\n");
    }
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

void
s390_initialize_trampoline (addr, fnaddr, cxt)
     rtx addr;
     rtx fnaddr;
     rtx cxt;
{
  emit_move_insn (gen_rtx 
		  (MEM, Pmode,
		   memory_address (Pmode, 
		   plus_constant (addr, (TARGET_64BIT ? 20 : 12) ))), cxt);
  emit_move_insn (gen_rtx
		  (MEM, Pmode,
		   memory_address (Pmode, 
		   plus_constant (addr, (TARGET_64BIT ? 28 : 16) ))), fnaddr);
}
