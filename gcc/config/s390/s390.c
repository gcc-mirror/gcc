/* Subroutines used for code generation on IBM S/390 and zSeries
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include "reload.h"
#include "toplev.h"
#include "basic-block.h"
#include "integrate.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "debug.h"


static bool s390_assemble_integer PARAMS ((rtx, unsigned int, int));
static int s390_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int s390_adjust_priority PARAMS ((rtx, int));

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER s390_assemble_integer

#undef  TARGET_ASM_FUNCTION_PROLOGUE 
#define TARGET_ASM_FUNCTION_PROLOGUE s390_function_prologue

#undef  TARGET_ASM_FUNCTION_EPILOGUE 
#define TARGET_ASM_FUNCTION_EPILOGUE s390_function_epilogue

#undef  TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN ""

#undef  TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ""

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST s390_adjust_cost

#undef  TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY s390_adjust_priority

struct gcc_target targetm = TARGET_INITIALIZER;

extern int reload_completed;

/* The alias set for prologue/epilogue register save/restore.  */
static int s390_sr_alias_set = 0;

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
  int pointer;
};

/* Structure containing information for prologue and epilogue.  */ 

struct s390_frame
{
  int frame_pointer_p;
  int return_reg_saved_p;
  int save_fprs_p;
  int first_save_gpr;
  int first_restore_gpr;
  int last_save_gpr;
  int arg_frame_offset;

  HOST_WIDE_INT frame_size;
};

static int s390_match_ccmode_set PARAMS ((rtx, enum machine_mode));
static int s390_branch_condition_mask PARAMS ((rtx));
static const char *s390_branch_condition_mnemonic PARAMS ((rtx, int));
static int check_mode PARAMS ((rtx, enum machine_mode *));
static int general_s_operand PARAMS ((rtx, enum machine_mode, int));
static int s390_decompose_address PARAMS ((rtx, struct s390_address *, int));
static int reg_used_in_mem_p PARAMS ((int, rtx));
static int addr_generation_dependency_p PARAMS ((rtx, rtx));
static void s390_split_branches PARAMS ((void));
static void find_constant_pool_ref PARAMS ((rtx, rtx *));
static void replace_constant_pool_ref PARAMS ((rtx *, rtx, rtx));
static void s390_chunkify_pool PARAMS ((void));
static int save_fprs_p PARAMS ((void));
static int find_unused_clobbered_reg PARAMS ((void));
static void s390_frame_info PARAMS ((struct s390_frame *));
static rtx save_fpr PARAMS ((rtx, int, int));
static rtx restore_fpr PARAMS ((rtx, int, int));
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
    case CCSmode:
      if (req_mode != CCSmode)
        return 0;
      break;
    case CCUmode:
      if (req_mode != CCUmode)
        return 0;
      break;
    case CCLmode:
      if (req_mode != CCLmode)
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

/* Given a comparison code OP (EQ, NE, etc.) and the operands 
   OP0 and OP1 of a COMPARE, return the mode to be used for the 
   comparison.  */

enum machine_mode
s390_select_ccmode (code, op0, op1) 
     enum rtx_code code;
     rtx op0;
     rtx op1;
{
  switch (code)
    {
      case EQ:
      case NE:
	if (GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS
	    || GET_CODE (op1) == NEG)
	  return CCLmode;

	return CCZmode;

      case LE:
      case LT:
      case GE:
      case GT:
      case UNORDERED:
      case ORDERED:
      case UNEQ:
      case UNLE:
      case UNLT:
      case UNGE:
      case UNGT:
      case LTGT:
	return CCSmode;

      case LEU:
      case LTU:
      case GEU:
      case GTU:
	return CCUmode;

      default:
	abort ();
    }
}

/* Return branch condition mask to implement a branch 
   specified by CODE.  */

static int
s390_branch_condition_mask (code)
    rtx code;
{ 
  const int CC0 = 1 << 3;
  const int CC1 = 1 << 2;
  const int CC2 = 1 << 1;
  const int CC3 = 1 << 0;

  if (GET_CODE (XEXP (code, 0)) != REG
      || REGNO (XEXP (code, 0)) != CC_REGNUM
      || XEXP (code, 1) != const0_rtx)
    abort ();

  switch (GET_MODE (XEXP (code, 0)))
    {
    case CCZmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
	case NE:	return CC1 | CC2 | CC3;
	default:
	  abort ();
        }
      break;

    case CCLmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0 | CC2;
	case NE:	return CC1 | CC3;
	case UNORDERED:	return CC2 | CC3;  /* carry */
	case ORDERED:	return CC0 | CC1;  /* no carry */
	default:
	  abort ();
        }
      break;

    case CCUmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
        case NE:	return CC1 | CC2 | CC3;
        case LTU:	return CC1;
        case GTU:	return CC2;
        case LEU:	return CC0 | CC1;
        case GEU:	return CC0 | CC2;
	default:
	  abort ();
        }
      break;

    case CCSmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
        case NE:	return CC1 | CC2 | CC3;
        case LT:	return CC1;
        case GT:	return CC2;
        case LE:	return CC0 | CC1;
        case GE:	return CC0 | CC2;
	case UNORDERED:	return CC3;
	case ORDERED:	return CC0 | CC1 | CC2;
	case UNEQ:	return CC0 | CC3;
        case UNLT:	return CC1 | CC3;
        case UNGT:	return CC2 | CC3;
        case UNLE:	return CC0 | CC1 | CC3;
        case UNGE:	return CC0 | CC2 | CC3;
	case LTGT:	return CC1 | CC2;
	default:
	  abort ();
        }

    default:
      abort ();
    }
}

/* If INV is false, return assembler mnemonic string to implement 
   a branch specified by CODE.  If INV is true, return mnemonic 
   for the corresponding inverted branch.  */

static const char *
s390_branch_condition_mnemonic (code, inv)
     rtx code;
     int inv;
{
  static const char *mnemonic[16] =
    {
      NULL, "o", "h", "nle",
      "l", "nhe", "lh", "ne",
      "e", "nlh", "he", "nl",
      "le", "nh", "no", NULL
    };

  int mask = s390_branch_condition_mask (code);

  if (inv)
    mask ^= 15;

  if (mask < 1 || mask > 14)
    abort ();

  return mnemonic[mask];
}

/* If OP is an integer constant of mode MODE with exactly one
   HImode subpart unequal to DEF, return the number of that 
   subpart.  As a special case, all HImode subparts of OP are
   equal to DEF, return zero.  Otherwise, return -1.  */

int
s390_single_hi (op, mode, def)
     rtx op;
     enum machine_mode mode;
     int def;
{
  if (GET_CODE (op) == CONST_INT)
    {
      unsigned HOST_WIDE_INT value;
      int n_parts = GET_MODE_SIZE (mode) / 2;
      int i, part = -1;

      for (i = 0; i < n_parts; i++)
        {
          if (i == 0)
            value = (unsigned HOST_WIDE_INT) INTVAL (op);
          else
            value >>= 16;

          if ((value & 0xffff) != (unsigned)(def & 0xffff))
            {
              if (part != -1)
                return -1;
              else
                part = i;
            }
        }

      return part == -1 ? 0 : (n_parts - 1 - part);
    }

  else if (GET_CODE (op) == CONST_DOUBLE
           && GET_MODE (op) == VOIDmode)
    {
      unsigned HOST_WIDE_INT value;
      int n_parts = GET_MODE_SIZE (mode) / 2;
      int i, part = -1;

      for (i = 0; i < n_parts; i++)
        {
          if (i == 0)
            value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (op);
          else if (i == HOST_BITS_PER_WIDE_INT / 16)
            value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (op);
          else
            value >>= 16;

          if ((value & 0xffff) != (unsigned)(def & 0xffff))
            {
              if (part != -1)
                return -1;
              else
                part = i;
            }
        }

      return part == -1 ? 0 : (n_parts - 1 - part);
    }

  return -1;      
}

/* Extract the HImode part number PART from integer 
   constant OP of mode MODE.  */

int
s390_extract_hi (op, mode, part)
    rtx op;
    enum machine_mode mode;
    int part;
{
  int n_parts = GET_MODE_SIZE (mode) / 2;
  if (part < 0 || part >= n_parts)
    abort();
  else
    part = n_parts - 1 - part;

  if (GET_CODE (op) == CONST_INT)
    {
      unsigned HOST_WIDE_INT value = (unsigned HOST_WIDE_INT) INTVAL (op);
      return ((value >> (16 * part)) & 0xffff);
    }
  else if (GET_CODE (op) == CONST_DOUBLE
           && GET_MODE (op) == VOIDmode)
    {
      unsigned HOST_WIDE_INT value;
      if (part < HOST_BITS_PER_WIDE_INT / 16)
        value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (op);
      else
        value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (op),
        part -= HOST_BITS_PER_WIDE_INT / 16;

      return ((value >> (16 * part)) & 0xffff); 
    }

  abort ();
}

/* If OP is an integer constant of mode MODE with exactly one
   QImode subpart unequal to DEF, return the number of that 
   subpart.  As a special case, all QImode subparts of OP are
   equal to DEF, return zero.  Otherwise, return -1.  */

int
s390_single_qi (op, mode, def)
     rtx op;
     enum machine_mode mode;
     int def;
{
  if (GET_CODE (op) == CONST_INT)
    {
      unsigned HOST_WIDE_INT value;
      int n_parts = GET_MODE_SIZE (mode);
      int i, part = -1;

      for (i = 0; i < n_parts; i++)
        {
          if (i == 0)
            value = (unsigned HOST_WIDE_INT) INTVAL (op);
          else
            value >>= 8;

          if ((value & 0xff) != (unsigned)(def & 0xff))
            {
              if (part != -1)
                return -1;
              else
                part = i;
            }
        }

      return part == -1 ? 0 : (n_parts - 1 - part);
    }

  else if (GET_CODE (op) == CONST_DOUBLE
           && GET_MODE (op) == VOIDmode)
    {
      unsigned HOST_WIDE_INT value;
      int n_parts = GET_MODE_SIZE (mode);
      int i, part = -1;

      for (i = 0; i < n_parts; i++)
        {
          if (i == 0)
            value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (op);
          else if (i == HOST_BITS_PER_WIDE_INT / 8)
            value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (op);
          else
            value >>= 8;

          if ((value & 0xff) != (unsigned)(def & 0xff))
            {
              if (part != -1)
                return -1;
              else
                part = i;
            }
        }

      return part == -1 ? 0 : (n_parts - 1 - part);
    }

  return -1;      
}

/* Extract the QImode part number PART from integer 
   constant OP of mode MODE.  */

int
s390_extract_qi (op, mode, part)
    rtx op;
    enum machine_mode mode;
    int part;
{
  int n_parts = GET_MODE_SIZE (mode);
  if (part < 0 || part >= n_parts)
    abort();
  else
    part = n_parts - 1 - part;

  if (GET_CODE (op) == CONST_INT)
    {
      unsigned HOST_WIDE_INT value = (unsigned HOST_WIDE_INT) INTVAL (op);
      return ((value >> (8 * part)) & 0xff);
    }
  else if (GET_CODE (op) == CONST_DOUBLE
           && GET_MODE (op) == VOIDmode)
    {
      unsigned HOST_WIDE_INT value;
      if (part < HOST_BITS_PER_WIDE_INT / 8)
        value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (op);
      else
        value = (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (op),
        part -= HOST_BITS_PER_WIDE_INT / 8;

      return ((value >> (8 * part)) & 0xff); 
    }

  abort ();
}


/* Change optimizations to be performed, depending on the 
   optimization level.

   LEVEL is the optimization level specified; 2 if `-O2' is
   specified, 1 if `-O' is specified, and 0 if neither is specified.

   SIZE is non-zero if `-Os' is specified and zero otherwise.  */

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

void
override_options ()
{
  /* Acquire a unique set number for our register saves and restores.  */
  s390_sr_alias_set = new_alias_set ();
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
  ADDR_REGS,    NO_REGS,   ADDR_REGS 
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

/* Return true if OP is constant.
   OP is the current operation.
   MODE is the current operation mode.  */

int
consttable_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return CONSTANT_P (op);
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

/* Helper routine to implement s_operand and s_imm_operand.
   OP is the current operation.
   MODE is the current operation mode.
   ALLOW_IMMEDIATE specifies whether immediate operands should
   be accepted or not.  */

static int
general_s_operand (op, mode, allow_immediate)
     register rtx op;
     enum machine_mode mode;
     int allow_immediate;
{
  struct s390_address addr;

  /* Call general_operand first, so that we don't have to
     check for many special cases.  */
  if (!general_operand (op, mode))
    return 0;

  /* Just like memory_operand, allow (subreg (mem ...))
     after reload.  */
  if (reload_completed 
      && GET_CODE (op) == SUBREG 
      && GET_CODE (SUBREG_REG (op)) == MEM)
    op = SUBREG_REG (op);

  switch (GET_CODE (op))
    {
      /* Constants that we are sure will be forced to the
         literal pool in reload are OK as s-operand.  Note
	 that we cannot call s390_preferred_reload_class here
	 because it might not be known yet at this point 
	 whether the current function is a leaf or not.  */
      case CONST_INT:
      case CONST_DOUBLE:
	if (!allow_immediate || reload_completed)
	  break;
	if (!legitimate_reload_constant_p (op))
	  return 1;
	if (!TARGET_64BIT)
	  return 1;
	break;

      /* Memory operands are OK unless they already use an
	 index register.  */
      case MEM:
	if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
	  return 1;
	if (s390_decompose_address (XEXP (op, 0), &addr, FALSE) 
	    && !addr.indx)
	  return 1;
	break;

      default:
	break;
    }

  return 0;
}

/* Return true if OP is a valid S-type operand.
   OP is the current operation.
   MODE is the current operation mode.  */

int
s_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return general_s_operand (op, mode, 0);
}

/* Return true if OP is a valid S-type operand or an immediate 
   operand that can be addressed as S-type operand by forcing 
   it into the literal pool.
   OP is the current operation.
   MODE is the current operation mode.  */

int
s_imm_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return general_s_operand (op, mode, 1);
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
  int i, off;


  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  /* Check, is base, or base + displacement.  */

  if (GET_CODE (src_addr) == REG)
    off = 0;
  else if (GET_CODE (src_addr) == PLUS
	   && GET_CODE (XEXP (src_addr, 0)) == REG 
	   && GET_CODE (XEXP (src_addr, 1)) == CONST_INT)
    {
      off = INTVAL (XEXP (src_addr, 1));
      src_addr = XEXP (src_addr, 0);
    }
  else
    return 0;

  if (src_addr == frame_pointer_rtx || src_addr == arg_pointer_rtx)
    return 0;

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
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1))
	     != off + i * UNITS_PER_WORD)
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
  int count = XVECLEN (op, 0);
  unsigned int src_regno;
  rtx dest_addr;
  int i, off;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  /* Check, is base, or base + displacement.  */

  if (GET_CODE (dest_addr) == REG)
    off = 0;
  else if (GET_CODE (dest_addr) == PLUS
	   && GET_CODE (XEXP (dest_addr, 0)) == REG 
	   && GET_CODE (XEXP (dest_addr, 1)) == CONST_INT)
    {
      off = INTVAL (XEXP (dest_addr, 1));
      dest_addr = XEXP (dest_addr, 0);
    }
  else
    return 0;

  if (dest_addr == frame_pointer_rtx || dest_addr == arg_pointer_rtx)
    return 0;

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
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1))
	     != off + i * UNITS_PER_WORD)
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
  /* Accept all non-symbolic constants.  */
  if (!SYMBOLIC_CONST (op))
    return 1;

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
  /* Accept all non-symbolic constants.  */
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

/* Returns true if the constant value OP is a legitimate general
   operand during and after reload.  The difference to 
   legitimate_constant_p is that this function will not accept
   a constant that would need to be forced to the literal pool
   before it can be used as operand.  */

int
legitimate_reload_constant_p (op)
     register rtx op;
{
  /* Accept l(g)hi operands.  */
  if (GET_CODE (op) == CONST_INT
      && CONST_OK_FOR_LETTER_P (INTVAL (op), 'K'))
    return 1;

  /* Accept lliXX operands.  */
  if (TARGET_64BIT
      && s390_single_hi (op, DImode, 0) >= 0)
  return 1;

  /* Accept larl operands.  */
  if (TARGET_64BIT
      && larl_operand (op, VOIDmode))
    return 1;

  /* If reload is completed, and we do not already have a
     literal pool, and OP must be forced to the literal 
     pool, then something must have gone wrong earlier.
     We *cannot* force the constant any more, because the
     prolog generation already decided we don't need to 
     set up the base register.  */
  if (reload_completed && !regs_ever_live[BASE_REGISTER])
    abort ();

  /* Everything else cannot be handled without reload.  */
  return 0;
}

/* Given an rtx OP being reloaded into a reg required to be in class CLASS,
   return the class of reg to actually use.  */

enum reg_class
s390_preferred_reload_class (op, class)
     rtx op;
     enum reg_class class;
{
  /* This can happen if a floating point constant is being
     reloaded into an integer register.  Leave well alone.  */
  if (GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT
      && class != FP_REGS)
    return class;

  switch (GET_CODE (op))
    {
      /* Constants we cannot reload must be forced into the
	 literal pool.  For constants we *could* handle directly,
	 it might still be preferable to put them in the pool and
	 use a memory-to-memory instruction.

	 However, try to avoid needlessly allocating a literal
	 pool in a routine that wouldn't otherwise need any.
	 Heuristically, we assume that 64-bit leaf functions
	 typically don't need a literal pool, all others do.  */
      case CONST_DOUBLE:
      case CONST_INT:
	if (!legitimate_reload_constant_p (op))
	  return NO_REGS;

	if (TARGET_64BIT && current_function_is_leaf)
	  return class;

	return NO_REGS;

      /* If a symbolic constant or a PLUS is reloaded,
	 it is most likely being used as an address, so
	 prefer ADDR_REGS.  If 'class' is not a superset
	 of ADDR_REGS, e.g. FP_REGS, reject this reload.  */
      case PLUS:
      case LABEL_REF:
      case SYMBOL_REF:
      case CONST:
	if (reg_class_subset_p (ADDR_REGS, class))
          return ADDR_REGS;
	else
	  return NO_REGS;

      default:
	break;
    }

  return class;
}

/* Return the register class of a scratch register needed to
   load IN into a register of class CLASS in MODE.

   We need a temporary when loading a PLUS expression which
   is not a legitimate operand of the LOAD ADDRESS instruction.  */

enum reg_class
s390_secondary_input_reload_class (class, mode, in)
     enum reg_class class ATTRIBUTE_UNUSED;
     enum machine_mode mode;
     rtx in;
{
  if (s390_plus_operand (in, mode))
    return ADDR_REGS;

  return NO_REGS;
}

/* Return true if OP is a PLUS that is not a legitimate
   operand for the LA instruction. 
   OP is the current operation.
   MODE is the current operation mode.  */

int
s390_plus_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (!check_mode (op, &mode) || mode != Pmode)
    return FALSE;

  if (GET_CODE (op) != PLUS)
    return FALSE;

  if (legitimate_la_operand_p (op))
    return FALSE;

  return TRUE;
}

/* Generate code to load SRC, which is PLUS that is not a
   legitimate operand for the LA instruction, into TARGET.
   SCRATCH may be used as scratch register.  */

void
s390_expand_plus_operand (target, src, scratch_in)
     register rtx target;
     register rtx src;
     register rtx scratch_in;
{
  rtx sum1, sum2, scratch;

  /* ??? reload apparently does not ensure that the scratch register
     and the target do not overlap.  We absolutely require this to be
     the case, however.  Therefore the reload_in[sd]i patterns ask for
     a double-sized scratch register, and if one part happens to be
     equal to the target, we use the other one.  */
  scratch = gen_rtx_REG (Pmode, REGNO (scratch_in));
  if (rtx_equal_p (scratch, target))
    scratch = gen_rtx_REG (Pmode, REGNO (scratch_in) + 1);

  /* src must be a PLUS; get its two operands.  */
  if (GET_CODE (src) != PLUS || GET_MODE (src) != Pmode)
    abort ();

  /* Check if any of the two operands is already scheduled
     for replacement by reload.  This can happen e.g. when
     float registers occur in an address.  */
  sum1 = find_replacement (&XEXP (src, 0));
  sum2 = find_replacement (&XEXP (src, 1));

  /* If one of the two operands is equal to the target,
     make it the first one.  If one is a constant, make
     it the second one.  */
  if (rtx_equal_p (target, sum2)
      || GET_CODE (sum1) == CONST_INT)
    {
      rtx tem = sum2;
      sum2 = sum1;
      sum1 = tem;
    }

  /* If the first operand is not an address register,
     we reload it into the target.  */
  if (true_regnum (sum1) < 1 || true_regnum (sum1) > 15)
    {
      emit_move_insn (target, sum1);
      sum1 = target;
    }

  /* Likewise for the second operand.  However, take
     care not to clobber the target if we already used
     it for the first operand.  Use the scratch instead.
     Also, allow an immediate offset if it is in range.  */
  if ((true_regnum (sum2) < 1 || true_regnum (sum2) > 15)
      && !(GET_CODE (sum2) == CONST_INT
           && INTVAL (sum2) >= 0 && INTVAL (sum2) < 4096))
    {
      if (!rtx_equal_p (target, sum1))
        {
          emit_move_insn (target, sum2);
          sum2 = target;
        }
      else
        {
          emit_move_insn (scratch, sum2);
          sum2 = scratch;
        }
    }

  /* Emit the LOAD ADDRESS pattern.  Note that reload of PLUS
     is only ever performed on addresses, so we can mark the
     sum as legitimate for LA in any case.  */
  src = gen_rtx_PLUS (Pmode, sum1, sum2);
  src = legitimize_la_operand (src);
  emit_insn (gen_rtx_SET (VOIDmode, target, src));
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
  int pointer = FALSE;

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
	  pointer = TRUE;
	}

      if (GET_CODE (base) != REG || GET_MODE (base) != Pmode)
	  return FALSE;

      if ((strict && ! REG_OK_FOR_BASE_STRICT_P (base))
	  || (! strict && ! REG_OK_FOR_BASE_NONSTRICT_P (base)))
	  return FALSE;
    
      if (REGNO (base) == BASE_REGISTER
	  || REGNO (base) == STACK_POINTER_REGNUM
	  || REGNO (base) == FRAME_POINTER_REGNUM
	  || ((reload_completed || reload_in_progress)
	      && frame_pointer_needed
	      && REGNO (base) == HARD_FRAME_POINTER_REGNUM)
          || (flag_pic
              && REGNO (base) == PIC_OFFSET_TABLE_REGNUM))
        pointer = TRUE;
    }

  /* Validate index register.  */
  if (indx)
    {
      if (GET_CODE (indx) == UNSPEC)
        {
          if (XVECLEN (indx, 0) != 1 || XINT (indx, 1) != 101)
	      return FALSE;
	  indx = XVECEXP (indx, 0, 0);
	  pointer = TRUE;
	}

      if (GET_CODE (indx) != REG || GET_MODE (indx) != Pmode)
	  return FALSE;

      if ((strict && ! REG_OK_FOR_BASE_STRICT_P (indx))
	  || (! strict && ! REG_OK_FOR_BASE_NONSTRICT_P (indx)))
	  return FALSE;
    
      if (REGNO (indx) == BASE_REGISTER
	  || REGNO (indx) == STACK_POINTER_REGNUM
	  || REGNO (indx) == FRAME_POINTER_REGNUM
	  || ((reload_completed || reload_in_progress)
	      && frame_pointer_needed
	      && REGNO (indx) == HARD_FRAME_POINTER_REGNUM)
          || (flag_pic
              && REGNO (indx) == PIC_OFFSET_TABLE_REGNUM))
        pointer = TRUE;
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

	  pointer = TRUE;
        }

      /* Accept chunkfied literal pool symbol references.  */
      else if (GET_CODE (disp) == CONST
               && GET_CODE (XEXP (disp, 0)) == MINUS
               && GET_CODE (XEXP (XEXP (disp, 0), 0)) == LABEL_REF
               && GET_CODE (XEXP (XEXP (disp, 0), 1)) == LABEL_REF)
        {
	  pointer = TRUE;
        }
 
      /* Likewise if a constant offset is present.  */
      else if (GET_CODE (disp) == CONST
               && GET_CODE (XEXP (disp, 0)) == PLUS
               && GET_CODE (XEXP (XEXP (disp, 0), 1)) == CONST_INT
               && GET_CODE (XEXP (XEXP (disp, 0), 0)) == MINUS
               && GET_CODE (XEXP (XEXP (XEXP (disp, 0), 0), 0)) == LABEL_REF
               && GET_CODE (XEXP (XEXP (XEXP (disp, 0), 0), 1)) == LABEL_REF)
        {
	  pointer = TRUE;
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

	  pointer = TRUE;
        }
    }

  if (!base && !indx)
    pointer = TRUE;
   
  if (out)
    {
      out->base = base;
      out->indx = indx;
      out->disp = disp;
      out->pointer = pointer;
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

/* Return 1 if OP is a valid operand for the LA instruction.
   In 31-bit, we need to prove that the result is used as an
   address, as LA performs only a 31-bit addition.  */

int
legitimate_la_operand_p (op)
     register rtx op;
{
  struct s390_address addr;
  if (!s390_decompose_address (op, &addr, FALSE))
    return FALSE;

  if (TARGET_64BIT || addr.pointer)
    return TRUE;

  return FALSE;
}

/* Return a modified variant of OP that is guaranteed to
   be accepted by legitimate_la_operand_p.  */

rtx
legitimize_la_operand (op)
     register rtx op;
{
  struct s390_address addr;
  if (!s390_decompose_address (op, &addr, FALSE))
    abort ();

  if (TARGET_64BIT || addr.pointer)
    return op;

  if (!addr.base)
    abort ();

  op = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr.base), 101);
  if (addr.indx)
    op = gen_rtx_PLUS (Pmode, op, addr.indx);
  if (addr.disp)
    op = gen_rtx_PLUS (Pmode, op, addr.disp);

  return op; 
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
             handled automatically ...  */
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
			  op0 = gen_rtx_CONST (Pmode, op0);
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
  rtx temp = no_new_pseudos ? operands[0] : gen_reg_rtx (Pmode);

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
  rtx constant_term = const0_rtx;

  if (flag_pic)
    {
      if (SYMBOLIC_CONST (x)
          || (GET_CODE (x) == PLUS 
              && (SYMBOLIC_CONST (XEXP (x, 0)) 
                  || SYMBOLIC_CONST (XEXP (x, 1)))))
	  x = legitimize_pic_address (x, 0);

      if (legitimate_address_p (mode, x, FALSE))
	return x;
    }

  x = eliminate_constant_term (x, &constant_term);

  if (GET_CODE (x) == PLUS)
    {
      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  x = gen_rtx_PLUS (Pmode, XEXP (x, 0), temp);
	}

      else if (GET_CODE (XEXP (x, 1)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 0), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  x = gen_rtx_PLUS (Pmode, temp, XEXP (x, 1));
	}
    }

  if (constant_term != const0_rtx)
    x = gen_rtx_PLUS (Pmode, x, constant_term);

  return x;
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler losage, recognize various UNSPEC sequences
   and turn them back into a direct symbol reference.  */

rtx
s390_simplify_dwarf_addr (orig_x)
     rtx orig_x;
{
  rtx x = orig_x, y;

  if (GET_CODE (x) != MEM)
    return orig_x;

  x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST
      && GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM)
    {
      y = XEXP (XEXP (x, 1), 0);
      if (GET_CODE (y) == UNSPEC
	  && XINT (y, 1) == 110)
	return XVECEXP (y, 0, 0);
      return orig_x;
    }

  if (GET_CODE (x) == CONST)
    {
      y = XEXP (x, 0);
      if (GET_CODE (y) == UNSPEC
	  && XINT (y, 1) == 111)
	return XVECEXP (y, 0, 0);
      return orig_x;
    }

  return orig_x;      
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
    case LABEL_REF:
    case CODE_LABEL:
    case SYMBOL_REF:
      output_addr_const (file, x);
      break;

    case UNSPEC:
      if (XVECLEN (x, 0) != 1)
        output_operand_lossage ("invalid UNSPEC as operand (1)");
      switch (XINT (x, 1))
        {
        case 100:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
          fprintf (file, "-.LT%X", s390_function_count);
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
          fprintf (file, "@PLT-.LT%X", s390_function_count);
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
    output_operand_lossage ("Cannot decompose address.");
 
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
    'O': print only the displacement of a memory reference.
    'R': print only the base register of a memory reference.
    'N': print the second word of a DImode operand.
    'M': print the second word of a TImode operand.

    'b': print integer X as if it's an unsigned byte.
    'x': print integer X as if it's an unsigned word.
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
      fprintf (file, s390_branch_condition_mnemonic (x, FALSE));
      return;

    case 'D':
      fprintf (file, s390_branch_condition_mnemonic (x, TRUE));
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
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) & 0xff);
      else if (code == 'x')
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) & 0xffff);
      else if (code == 'h')
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, ((INTVAL (x) & 0xffff) ^ 0x8000) - 0x8000);
      else
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) != VOIDmode)
        abort ();
      if (code == 'b')
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x) & 0xff);
      else if (code == 'x')
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x) & 0xffff);
      else if (code == 'h')
        fprintf (file, HOST_WIDE_INT_PRINT_DEC, ((CONST_DOUBLE_LOW (x) & 0xffff) ^ 0x8000) - 0x8000);
      else
        abort ();
      break;

    default:
      fatal_insn ("UNKNOWN in print_operand !?", x);
      break;
    }
}

/* Target hook for assembling integer objects.  We need to define it
   here to work a round a bug in some versions of GAS, which couldn't
   handle values smaller than INT_MIN when printed in decimal.  */

static bool
s390_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (size == 8 && aligned_p
      && GET_CODE (x) == CONST_INT && INTVAL (x) < INT_MIN)
    {
      fputs ("\t.quad\t", asm_out_file);
      fprintf (asm_out_file, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      putc ('\n', asm_out_file);
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
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
  else if (code == SET 
	   && GET_CODE (SET_DEST (x)) == PC)
    {
      if (refers_to_regno_p (regno, regno+1,
			     SET_SRC (x), 0))
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

/* Returns true if expression DEP_RTX sets an address register
   used by instruction INSN to address memory.  */

static int 
addr_generation_dependency_p (dep_rtx, insn)
     rtx dep_rtx; 
     rtx insn;
{
  rtx target, pat;

  if (GET_CODE (dep_rtx) == SET)
    {
      target = SET_DEST (dep_rtx);
      
      if (GET_CODE (target) == REG)
	{
	  int regno = REGNO (target);

	  if (get_attr_type (insn) == TYPE_LA)
	    {
	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == PARALLEL)
		{
		  if (XVECLEN (pat, 0) != 2)
		    abort();
		  pat = XVECEXP (pat, 0, 0);
		}
	      if (GET_CODE (pat) == SET)
		return refers_to_regno_p (regno, regno+1, SET_SRC (pat), 0);
	      else
		abort();
	    }
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
   between setting and using the register to avoid pipeline stalls.  
   An exception is the LA instruction. An address generated by LA can
   be used by introducing only a one cycle stall on the pipeline.  */

static int
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

  dep_rtx = PATTERN (dep_insn);

  if (GET_CODE (dep_rtx) == SET)
    {
      if (addr_generation_dependency_p (dep_rtx, insn))
	{
	  cost += (get_attr_type (dep_insn) == TYPE_LA) ? 1 : 4;  
	  if (DEBUG_SCHED)
	    {
	      fprintf (stderr, "\n\nAddress dependency detected: cost %d\n",
		       cost);
	      debug_rtx (dep_insn);
	      debug_rtx (insn);
	    }
	}
    }
  else if (GET_CODE (dep_rtx) == PARALLEL)
    {
      for (i = 0; i < XVECLEN (dep_rtx, 0); i++)
	{
	  if (addr_generation_dependency_p (XVECEXP (dep_rtx, 0, i),
					    insn))
	    {
	      cost += (get_attr_type (dep_insn) == TYPE_LA) ? 1 : 4;  
	      if (DEBUG_SCHED)
		{
		  fprintf (stderr, "\n\nAddress dependency detected: cost %d\n"
			   ,cost);
		  debug_rtx (dep_insn);
		  debug_rtx (insn);
		}
	    }
	}
    }

  return cost;
}


/* A C statement (sans semicolon) to update the integer scheduling priority
   INSN_PRIORITY (INSN).  Reduce the priority to execute the INSN earlier,
   increase the priority to execute INSN later.  Do not define this macro if
   you do not need to adjust the scheduling priorities of insns. 

   A LA instruction maybe scheduled later, since the pipeline bypasses the
   calculated value.  */

static int
s390_adjust_priority (insn, priority)
     rtx insn ATTRIBUTE_UNUSED;
     int priority;
{
  if (! INSN_P (insn))
    return priority;

  if (GET_CODE (PATTERN (insn)) == USE 
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return priority;
  
  switch (get_attr_type (insn))
    {
    default:
      break;
      
    case TYPE_LA:
      if (priority >= 0 && priority < 0x01000000)
	priority <<= 3;
      break;
    case TYPE_LM:
      /* LM in epilogue should never be scheduled. This
	 is due to literal access done in function body.
	 The usage of register 13 is not mentioned explicitly,
	 leading to scheduling 'LM' accross this instructions.  
      */ 
      priority = 0x7fffffff;
      break;
    }
  
  return priority;
}


/* Split all branches that exceed the maximum distance.  */

static void 
s390_split_branches (void)
{
  rtx temp_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);
  rtx insn, pat, label, target, jump, tmp;

  /* In 64-bit mode we can jump +- 4GB.  */

  if (TARGET_64BIT)
    return;

  /* Find all branches that exceed 64KB, and split them.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) != JUMP_INSN)
	continue;

      pat = PATTERN (insn);
      if (GET_CODE (pat) != SET)
	continue;

      if (GET_CODE (SET_SRC (pat)) == LABEL_REF) 
	{
	  label = SET_SRC (pat);
	} 
      else if (GET_CODE (SET_SRC (pat)) == IF_THEN_ELSE) 
	{
	  if (GET_CODE (XEXP (SET_SRC (pat), 1)) == LABEL_REF) 
	    label = XEXP (SET_SRC (pat), 1);
          else if (GET_CODE (XEXP (SET_SRC (pat), 2)) == LABEL_REF) 
            label = XEXP (SET_SRC (pat), 2);
	  else
	    continue;
        }
      else
	continue;

      if (get_attr_length (insn) == 4)
	continue;

      if (flag_pic)
	{
	  target = gen_rtx_UNSPEC (SImode, gen_rtvec (1, label), 100);
	  target = gen_rtx_CONST (SImode, target);
	  target = force_const_mem (SImode, target);
	  jump = gen_rtx_REG (Pmode, BASE_REGISTER);
	  jump = gen_rtx_PLUS (Pmode, jump, temp_reg);
	}
      else
	{
	  target = force_const_mem (Pmode, label);
	  jump = temp_reg;
	}

      if (GET_CODE (SET_SRC (pat)) == IF_THEN_ELSE)
	{
	  if (GET_CODE (XEXP (SET_SRC (pat), 1)) == LABEL_REF)
	    jump = gen_rtx_IF_THEN_ELSE (VOIDmode, XEXP (SET_SRC (pat), 0),
					 jump, pc_rtx);
	  else
	    jump = gen_rtx_IF_THEN_ELSE (VOIDmode, XEXP (SET_SRC (pat), 0),
					 pc_rtx, jump);
	}

      tmp = emit_insn_before (gen_rtx_SET (Pmode, temp_reg, target), insn);
      INSN_ADDRESSES_NEW (tmp, -1);

      tmp = emit_jump_insn_before (gen_rtx_SET (VOIDmode, pc_rtx, jump), insn);
      INSN_ADDRESSES_NEW (tmp, -1);

      remove_insn (insn);
      insn = tmp;
    }
}


/* Find a literal pool symbol referenced in RTX X, and store 
   it at REF.  Will abort if X contains references to more than 
   one such pool symbol; multiple references to the same symbol
   are allowed, however. 

   The rtx pointed to by REF must be initialized to NULL_RTX 
   by the caller before calling this routine.  */

static void
find_constant_pool_ref (x, ref)
     rtx x;
     rtx *ref;
{
  int i, j;
  const char *fmt;

  if (GET_CODE (x) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (x))
    {
      if (*ref == NULL_RTX)
        *ref = x;
      else if (*ref != x)
        abort();
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        {
          find_constant_pool_ref (XEXP (x, i), ref);
        }
      else if (fmt[i] == 'E')
        {
          for (j = 0; j < XVECLEN (x, i); j++)
            find_constant_pool_ref (XVECEXP (x, i, j), ref);
        }
    }
}

/* Replace every reference to the literal pool symbol REF
   in X by the address ADDR.  Fix up MEMs as required.  */

static void
replace_constant_pool_ref (x, ref, addr)
     rtx *x;
     rtx ref;
     rtx addr;
{
  int i, j;
  const char *fmt;

  if (*x == ref)
    abort ();

  /* Literal pool references can only occur inside a MEM ...  */
  if (GET_CODE (*x) == MEM)
    {
      rtx memref = XEXP (*x, 0);

      if (memref == ref)
	{
	  *x = replace_equiv_address (*x, addr);
	  return;
	}

      if (GET_CODE (memref) == CONST
	  && GET_CODE (XEXP (memref, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (memref, 0), 1)) == CONST_INT
	  && XEXP (XEXP (memref, 0), 0) == ref)
	{
	  HOST_WIDE_INT off = INTVAL (XEXP (XEXP (memref, 0), 1));
	  *x = replace_equiv_address (*x, plus_constant (addr, off));
	  return;
	}
    }

  /* ... or a load-address type pattern.  */
  if (GET_CODE (*x) == SET)
    {
      rtx addrref = SET_SRC (*x);

      if (addrref == ref)
	{
	  SET_SRC (*x) = addr;
	  return;
	}

      if (GET_CODE (addrref) == CONST
	  && GET_CODE (XEXP (addrref, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (addrref, 0), 1)) == CONST_INT
	  && XEXP (XEXP (addrref, 0), 0) == ref)
	{
	  HOST_WIDE_INT off = INTVAL (XEXP (XEXP (addrref, 0), 1));
	  SET_SRC (*x) = plus_constant (addr, off);
	  return;
	}
    }

  fmt = GET_RTX_FORMAT (GET_CODE (*x));
  for (i = GET_RTX_LENGTH (GET_CODE (*x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        {
          replace_constant_pool_ref (&XEXP (*x, i), ref, addr);
        }
      else if (fmt[i] == 'E')
        {
          for (j = 0; j < XVECLEN (*x, i); j++)
            replace_constant_pool_ref (&XVECEXP (*x, i, j), ref, addr);
        }
    }
}

/* We keep a list of constants we which we have to add to internal
   constant tables in the middle of large functions.  */

#define NR_C_MODES 6
enum machine_mode constant_modes[NR_C_MODES] = 
{
  DFmode, DImode,
  SFmode, SImode,
  HImode,
  QImode
};

rtx (*gen_consttable[NR_C_MODES])(rtx) =
{
  gen_consttable_df, gen_consttable_di,
  gen_consttable_sf, gen_consttable_si,
  gen_consttable_hi,
  gen_consttable_qi
};

struct constant
{
  struct constant *next;
  rtx value;
  rtx label;
};

struct constant_pool
{
  struct constant_pool *next;
  rtx first_insn;
  rtx last_insn;

  struct constant *constants[NR_C_MODES];
  rtx label;
  int size;
};

static struct constant_pool *s390_start_pool PARAMS ((struct constant_pool **, rtx));
static void s390_end_pool PARAMS ((struct constant_pool *, rtx));
static struct constant_pool *s390_find_pool PARAMS ((struct constant_pool *, rtx));
static rtx s390_add_pool PARAMS ((struct constant_pool *, rtx, enum machine_mode));
static rtx s390_dump_pool PARAMS ((struct constant_pool *));
static void s390_free_pool PARAMS ((struct constant_pool *));

/* Create new constant pool covering instructions starting at INSN
   and chain it to the end of POOL_LIST.  */

static struct constant_pool *
s390_start_pool (pool_list, insn)
     struct constant_pool **pool_list;
     rtx insn;
{
  struct constant_pool *pool, **prev;
  int i;

  pool = (struct constant_pool *) xmalloc (sizeof *pool);
  pool->next = NULL;
  for (i = 0; i < NR_C_MODES; i++)
    pool->constants[i] = NULL;

  pool->label = gen_label_rtx ();
  pool->first_insn = insn;
  pool->last_insn = NULL_RTX;
  pool->size = 0;
 
  for (prev = pool_list; *prev; prev = &(*prev)->next)
    ;
  *prev = pool;

  return pool;
}

/* End range of instructions covered by POOL at INSN.  */

static void
s390_end_pool (pool, insn)
     struct constant_pool *pool;
     rtx insn;
{
  pool->last_insn = insn;
}

/* Return pool out of POOL_LIST that covers INSN.  */

static struct constant_pool *
s390_find_pool (pool_list, insn)
     struct constant_pool *pool_list;
     rtx insn;
{
  int addr = INSN_ADDRESSES (INSN_UID (insn));
  struct constant_pool *pool;

  if (addr == -1)
    return NULL;

  for (pool = pool_list; pool; pool = pool->next)
    if (INSN_ADDRESSES (INSN_UID (pool->first_insn)) <= addr
        && (pool->last_insn == NULL_RTX
            || INSN_ADDRESSES (INSN_UID (pool->last_insn)) > addr))
      break;

  return pool;
}

/* Add constant VAL of mode MODE to the constant pool POOL.
   Return an RTX describing the distance from the start of
   the pool to the location of the new constant.  */

static rtx
s390_add_pool (pool, val, mode)
     struct constant_pool *pool;
     rtx val;
     enum machine_mode mode;
{
  struct constant *c;
  rtx offset;
  int i;

  for (i = 0; i < NR_C_MODES; i++)
    if (constant_modes[i] == mode)
      break;
  if (i == NR_C_MODES)
    abort ();

  for (c = pool->constants[i]; c != NULL; c = c->next)
    if (rtx_equal_p (val, c->value))
      break;

  if (c == NULL)
    {
      c = (struct constant *) xmalloc (sizeof *c);
      c->value = val;
      c->label = gen_label_rtx ();
      c->next = pool->constants[i];
      pool->constants[i] = c;
      pool->size += GET_MODE_SIZE (mode);
    }

  offset = gen_rtx_MINUS (Pmode, gen_rtx_LABEL_REF (Pmode, c->label), 
				 gen_rtx_LABEL_REF (Pmode, pool->label));
  offset = gen_rtx_CONST (Pmode, offset);
  return offset;
}

/* Dump out the constants in POOL.  */

static rtx
s390_dump_pool (pool)
     struct constant_pool *pool;
{
  struct constant *c;
  rtx insn;
  int i;

  /* Select location to put literal pool.  */
  if (TARGET_64BIT)
    insn = get_last_insn ();
  else
    insn = pool->last_insn? pool->last_insn : get_last_insn ();

  /* Pool start insn switches to proper section 
     and guarantees necessary alignment.  */
  if (TARGET_64BIT)
    insn = emit_insn_after (gen_pool_start_64 (), insn);
  else
    insn = emit_insn_after (gen_pool_start_31 (), insn);
  INSN_ADDRESSES_NEW (insn, -1);

  insn = emit_label_after (pool->label, insn);
  INSN_ADDRESSES_NEW (insn, -1);

  /* Dump constants in descending alignment requirement order,
     ensuring proper alignment for every constant.  */
  for (i = 0; i < NR_C_MODES; i++)
    for (c = pool->constants[i]; c; c = c->next)
      {
	insn = emit_label_after (c->label, insn);
	INSN_ADDRESSES_NEW (insn, -1);
	insn = emit_insn_after (gen_consttable[i] (c->value), insn);
	INSN_ADDRESSES_NEW (insn, -1);
      }

  /* Pool end insn switches back to previous section 
     and guarantees necessary alignment.  */
  if (TARGET_64BIT)
    insn = emit_insn_after (gen_pool_end_64 (), insn);
  else
    insn = emit_insn_after (gen_pool_end_31 (), insn);
  INSN_ADDRESSES_NEW (insn, -1);

  insn = emit_barrier_after (insn);
  INSN_ADDRESSES_NEW (insn, -1);

  return insn;
}

/* Free all memory used by POOL.  */

static void
s390_free_pool (pool)
     struct constant_pool *pool;
{
  int i;

  for (i = 0; i < NR_C_MODES; i++)
    {
      struct constant *c = pool->constants[i];
      while (c != NULL)
	{
	  struct constant *next = c->next;
	  free (c);
	  c = next;
	}
    }

  free (pool);
} 

/* Used in s390.md for branch length calculation.  */
int s390_pool_overflow = 0;

/* Chunkify the literal pool if required.  */

#define S390_POOL_CHUNK_MIN	0xc00
#define S390_POOL_CHUNK_MAX	0xe00

static void 
s390_chunkify_pool (void)
{
  rtx base_reg = gen_rtx_REG (Pmode, 
			      TARGET_64BIT? BASE_REGISTER : RETURN_REGNUM);

  struct constant_pool *curr_pool = NULL, *pool_list = NULL;
  int extra_size = 0;
  bitmap far_labels;
  rtx insn;

  /* Do we need to chunkify the literal pool?  */

  if (get_pool_size () < S390_POOL_CHUNK_MAX)
    return;

  /* Scan all insns and move literals to pool chunks.
     Replace all occurrances of literal pool references
     by explicit references to pool chunk entries.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN)
	{
	  rtx addr, pool_ref = NULL_RTX;
	  find_constant_pool_ref (PATTERN (insn), &pool_ref);
	  if (pool_ref)
	    {
	      if (!curr_pool)
		curr_pool = s390_start_pool (&pool_list, insn);

	      addr = s390_add_pool (curr_pool, get_pool_constant (pool_ref), 
					       get_pool_mode (pool_ref));

	      addr = gen_rtx_PLUS (Pmode, base_reg, addr);
	      replace_constant_pool_ref (&PATTERN (insn), pool_ref, addr);
	      INSN_CODE (insn) = -1;
	    }
	}

      if (!curr_pool 
	  || INSN_ADDRESSES_SIZE () <= (size_t) INSN_UID (insn)
          || INSN_ADDRESSES (INSN_UID (insn)) == -1)
	continue;

      if (TARGET_64BIT)
	{
	  if (curr_pool->size < S390_POOL_CHUNK_MAX)
	    continue;

	  s390_end_pool (curr_pool, insn);
	  curr_pool = NULL;
	}
      else
	{
          int chunk_size = INSN_ADDRESSES (INSN_UID (insn))
		 	   - INSN_ADDRESSES (INSN_UID (curr_pool->first_insn))
			 + extra_size;

	  /* We will later have to insert base register reload insns.
	     Those will have an effect on code size, which we need to
	     consider here.  This calculation makes rather pessimistic
	     worst-case assumptions.  */
	  if (GET_CODE (insn) == CODE_LABEL
	      || GET_CODE (insn) == JUMP_INSN)
	    extra_size += 6;
	  else if (GET_CODE (insn) == CALL_INSN)
	    extra_size += 4;

	  if (chunk_size < S390_POOL_CHUNK_MIN
	      && curr_pool->size < S390_POOL_CHUNK_MIN)
	    continue;

	  /* Pool chunks can only be inserted after BARRIERs ...  */
	  if (GET_CODE (insn) == BARRIER)
	    {
	      s390_end_pool (curr_pool, insn);
	      curr_pool = NULL;
	      extra_size = 0;
	    }

	  /* ... so if we don't find one in time, create one.  */
          else if ((chunk_size > S390_POOL_CHUNK_MAX
	           || curr_pool->size > S390_POOL_CHUNK_MAX)
	          && (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN))
	    {
	      int addr = INSN_ADDRESSES (INSN_UID (insn));
              rtx label, jump, barrier;

 	      label = gen_label_rtx ();
	      jump = emit_jump_insn_after (gen_jump (label), insn);
	      barrier = emit_barrier_after (jump);
	      insn = emit_label_after (label, barrier);
	      JUMP_LABEL (jump) = label;
	      LABEL_NUSES (label) = 1;

	      INSN_ADDRESSES_NEW (jump, addr+1);
	      INSN_ADDRESSES_NEW (barrier, addr+1);
	      INSN_ADDRESSES_NEW (insn, -1);

	      s390_end_pool (curr_pool, barrier);
	      curr_pool = NULL;
	      extra_size = 0;
	    }
	}
    }

  /* Dump out all literal pools.  */

  for (curr_pool = pool_list; curr_pool; curr_pool = curr_pool->next)
    s390_dump_pool (curr_pool);


  /* Find all labels that are branched into 
     from an insn belonging to a different chunk.  */

  far_labels = BITMAP_XMALLOC ();

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      /* Labels marked with LABEL_PRESERVE_P can be target
	 of non-local jumps, so we have to mark them.
	 The same holds for named labels.

	 Don't do that, however, if it is the label before
	 a jump table.  */

      if (GET_CODE (insn) == CODE_LABEL 
	  && (LABEL_PRESERVE_P (insn) || LABEL_NAME (insn)))
	{
	  rtx vec_insn = next_real_insn (insn);
	  rtx vec_pat = vec_insn && GET_CODE (vec_insn) == JUMP_INSN ? 
			PATTERN (vec_insn) : NULL_RTX;
	  if (!vec_pat
	      || !(GET_CODE (vec_pat) == ADDR_VEC
		   || GET_CODE (vec_pat) == ADDR_DIFF_VEC))
	    bitmap_set_bit (far_labels, CODE_LABEL_NUMBER (insn));
	}

      /* If we have a direct jump (conditional or unconditional)
	 or a casesi jump, check all potential targets.  */
      else if (GET_CODE (insn) == JUMP_INSN) 
	{
          rtx pat = PATTERN (insn);
          if (GET_CODE (pat) == SET) 
            {
	      rtx label = 0;

              if (GET_CODE (SET_SRC (pat)) == LABEL_REF) 
	        {
	          label = XEXP (SET_SRC (pat), 0);
	        } 
              else if (GET_CODE (SET_SRC (pat)) == IF_THEN_ELSE) 
	        {
	          if (GET_CODE (XEXP (SET_SRC (pat), 1)) == LABEL_REF) 
	            label = XEXP (XEXP (SET_SRC (pat), 1), 0);
	          else if (GET_CODE (XEXP (SET_SRC (pat), 2)) == LABEL_REF) 
	            label = XEXP (XEXP (SET_SRC (pat), 2), 0);
	        }

	      if (label)
		{
	          if (s390_find_pool (pool_list, label) 
		      != s390_find_pool (pool_list, insn))
		    bitmap_set_bit (far_labels, CODE_LABEL_NUMBER (label));
		}
            } 
	  else if (GET_CODE (pat) == PARALLEL
		   && XVECLEN (pat, 0) == 2
		   && GET_CODE (XVECEXP (pat, 0, 0)) == SET
		   && GET_CODE (XVECEXP (pat, 0, 1)) == USE
		   && GET_CODE (XEXP (XVECEXP (pat, 0, 1), 0)) == LABEL_REF)
	    {
	      /* Find the jump table used by this casesi jump.  */
	      rtx vec_label = XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0);
	      rtx vec_insn = next_real_insn (vec_label);
	      rtx vec_pat = vec_insn && GET_CODE (vec_insn) == JUMP_INSN ? 
			    PATTERN (vec_insn) : NULL_RTX;
	      if (vec_pat
		  && (GET_CODE (vec_pat) == ADDR_VEC
		      || GET_CODE (vec_pat) == ADDR_DIFF_VEC))
		{
		  int i, diff_p = GET_CODE (vec_pat) == ADDR_DIFF_VEC;

		  for (i = 0; i < XVECLEN (vec_pat, diff_p); i++)
		    {
		      rtx label = XEXP (XVECEXP (vec_pat, diff_p, i), 0);

		      if (s390_find_pool (pool_list, label) 
			  != s390_find_pool (pool_list, insn))
			bitmap_set_bit (far_labels, CODE_LABEL_NUMBER (label));
		    }
		}
	    }
        }
    }

  /* Insert base register reload insns before every pool.  */

  for (curr_pool = pool_list; curr_pool; curr_pool = curr_pool->next)
    if (TARGET_64BIT)
      {
	rtx pool_ref = gen_rtx_LABEL_REF (Pmode, curr_pool->label);
	rtx new_insn = gen_rtx_SET (Pmode, base_reg, pool_ref);
	rtx insn = curr_pool->first_insn;
        INSN_ADDRESSES_NEW (emit_insn_before (new_insn, insn), -1);
      }
    else
      {
	rtx new_insn = gen_reload_base (base_reg, curr_pool->label);
	rtx insn = curr_pool->first_insn;
        INSN_ADDRESSES_NEW (emit_insn_before (new_insn, insn), -1);
      }

  /* Insert base register reload insns at every far label.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == CODE_LABEL 
        && bitmap_bit_p (far_labels, CODE_LABEL_NUMBER (insn)))
      {
	struct constant_pool *pool = s390_find_pool (pool_list, insn);
	if (pool)
	  {
	    if (TARGET_64BIT)
	      {
		rtx pool_ref = gen_rtx_LABEL_REF (Pmode, pool->label);
		rtx new_insn = gen_rtx_SET (Pmode, base_reg, pool_ref);
	        INSN_ADDRESSES_NEW (emit_insn_after (new_insn, insn), -1);
	      }
	    else
	      {
		rtx new_insn = gen_reload_base (base_reg, pool->label);
	        INSN_ADDRESSES_NEW (emit_insn_after (new_insn, insn), -1);
 	      }
	  }
      }

  /* Insert base register reload insns after every call if necessary.  */

  if (REGNO (base_reg) == RETURN_REGNUM)
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == CALL_INSN)
	 {
	   struct constant_pool *pool = s390_find_pool (pool_list, insn);
	   if (pool)
	     {
	       rtx new_insn = gen_reload_base2 (base_reg, pool->label);
	       INSN_ADDRESSES_NEW (emit_insn_after (new_insn, insn), -1);
	     }
	  }


  /* Recompute insn addresses.  */

  s390_pool_overflow = 1;
  init_insn_lengths ();
  shorten_branches (get_insns ());
  s390_pool_overflow = 0;

  /* Insert base register reload insns after far branches.  */

  if (!TARGET_64BIT)
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == JUMP_INSN
	  && GET_CODE (PATTERN (insn)) == SET
	  && get_attr_length (insn) >= 12)
	{
	  struct constant_pool *pool = s390_find_pool (pool_list, insn);
	  if (pool)
	    {
	      rtx new_insn = gen_reload_base (base_reg, pool->label);
	      INSN_ADDRESSES_NEW (emit_insn_after (new_insn, insn), -1);
	    }
	}


  /* Free all memory.  */

  while (pool_list)
    {
      struct constant_pool *next = pool_list->next;
      s390_free_pool (pool_list);
      pool_list = next;
    }

  BITMAP_XFREE (far_labels);
}


/* Index of constant pool chunk that is currently being processed.
   Set to -1 before function output has started.  */
int s390_pool_count = -1;

/* Number of elements of current constant pool.  */
int s390_nr_constants;

/* Output main constant pool to stdio stream FILE.  */ 

void
s390_output_constant_pool (file)
     FILE *file;
{
  /* Output constant pool.  */
  if (s390_nr_constants)
    {
      if (TARGET_64BIT)
	{
	  fprintf (file, "\tlarl\t%s,.LT%X\n", reg_names[BASE_REGISTER],
		   s390_function_count);
	  readonly_data_section ();
	  ASM_OUTPUT_ALIGN (file, 3);
	}
      else
	{
	  fprintf (file, "\tbras\t%s,.LTN%X\n", reg_names[BASE_REGISTER],
		   s390_function_count);
	}
      fprintf (file, ".LT%X:\n", s390_function_count);

      s390_pool_count = 0;
      output_constant_pool (current_function_name, current_function_decl);
      s390_pool_count = -1;

      if (TARGET_64BIT)
	function_section (current_function_decl);
      else
        fprintf (file, ".LTN%X:\n", s390_function_count);
    }
}


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

/* Find first call clobbered register unsused in a function.
   This could be used as base register in a leaf function
   or for holding the return address before epilogue.  */

static int
find_unused_clobbered_reg ()
{
  int i;
  for (i = 0; i < 6; i++)
    if (!regs_ever_live[i])
      return i;
  return 0;
}

/* Fill FRAME with info about frame of current function.  */

static void
s390_frame_info (frame)
     struct s390_frame *frame;
{
  int i, j;
  HOST_WIDE_INT fsize = get_frame_size ();

  if (fsize > 0x7fff0000)
    fatal_error ("Total size of local variables exceeds architecture limit.");

  /* fprs 8 - 15 are caller saved for 64 Bit ABI.  */
  frame->save_fprs_p = save_fprs_p ();

  frame->frame_size = fsize + frame->save_fprs_p * 64;

  /* Does function need to setup frame and save area.  */
  
  if (! current_function_is_leaf
      || frame->frame_size > 0
      || current_function_calls_alloca 
      || current_function_stdarg
      || current_function_varargs)
    frame->frame_size += STARTING_FRAME_OFFSET;

  /* If we need to allocate a frame, the stack pointer is changed.  */ 

  if (frame->frame_size > 0)
    regs_ever_live[STACK_POINTER_REGNUM] = 1;

  /* If the literal pool might overflow, the return register might
     be used as temp literal pointer.  */

  if (!TARGET_64BIT && get_pool_size () >= S390_POOL_CHUNK_MAX / 2)
    regs_ever_live[RETURN_REGNUM] = 1;

  /* If there is (possibly) any pool entry, we need to 
     load base register.  */

  if (get_pool_size () 
      || !CONST_OK_FOR_LETTER_P (frame->frame_size, 'K')
      || (!TARGET_64BIT && current_function_uses_pic_offset_table))
    regs_ever_live[BASE_REGISTER] = 1; 

  /* If we need the GOT pointer, remember to save/restore it.  */

  if (current_function_uses_pic_offset_table)
    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;

  /* Frame pointer needed.   */
    
  frame->frame_pointer_p = frame_pointer_needed;

  /* Find first and last gpr to be saved.  */
  
  for (i = 6; i < 16; i++)
    if (regs_ever_live[i])
      break;

  for (j = 15; j > i; j--)
    if (regs_ever_live[j])
      break;
  
  if (i == 16)
    {
      /* Nothing to save / restore.  */ 
      frame->first_save_gpr = -1;
      frame->first_restore_gpr = -1;
      frame->last_save_gpr = -1;
      frame->return_reg_saved_p = 0;
    }
  else
    {
      /* Save / Restore from gpr i to j.  */
      frame->first_save_gpr = i;
      frame->first_restore_gpr = i;
      frame->last_save_gpr  = j;
      frame->return_reg_saved_p = (j >= RETURN_REGNUM && i <= RETURN_REGNUM);
    }

  if (current_function_stdarg || current_function_varargs)
    {
      /* Varargs function need to save from gpr 2 to gpr 15.  */
      frame->first_save_gpr = 2;
    }
}

/* Return offset between argument pointer and frame pointer 
   initially after prologue.  */

int 
s390_arg_frame_offset ()
{
  struct s390_frame frame;

  /* Compute frame_info.  */

  s390_frame_info (&frame);

  return frame.frame_size + STACK_POINTER_OFFSET;
}

/* Emit insn to save fpr REGNUM at offset OFFSET relative
   to register BASE.  Return generated insn.  */ 

static rtx
save_fpr (base, offset, regnum)
     rtx base;
     int offset;
     int regnum;     
{
  rtx addr;
  addr = gen_rtx_MEM (DFmode, plus_constant (base, offset));
  set_mem_alias_set (addr, s390_sr_alias_set);

  return emit_move_insn (addr, gen_rtx_REG (DFmode, regnum));
}

/* Emit insn to restore fpr REGNUM from offset OFFSET relative
   to register BASE.  Return generated insn.  */ 

static rtx
restore_fpr (base, offset, regnum)
     rtx base;
     int offset;
     int regnum;
{
  rtx addr;
  addr = gen_rtx_MEM (DFmode, plus_constant (base, offset));
  set_mem_alias_set (addr, s390_sr_alias_set);

  return emit_move_insn (gen_rtx_REG (DFmode, regnum), addr);
}

/* Output the function prologue assembly code to the 
   stdio stream FILE.  The local frame size is passed
   in LSIZE.  */

void
s390_function_prologue (file, lsize)
     FILE *file ATTRIBUTE_UNUSED;
     HOST_WIDE_INT lsize ATTRIBUTE_UNUSED;
{
  s390_chunkify_pool ();
  s390_split_branches ();
}

/* Output the function epilogue assembly code to the 
   stdio stream FILE.  The local frame size is passed
   in LSIZE.  */

void
s390_function_epilogue (file, lsize)
     FILE *file ATTRIBUTE_UNUSED;
     HOST_WIDE_INT lsize ATTRIBUTE_UNUSED;
{
  current_function_uses_pic_offset_table = 0;
  s390_function_count++;
}

/* Expand the prologue into a bunch of separate insns.  */

void
s390_emit_prologue ()
{
  struct s390_frame frame;
  rtx insn, addr;
  rtx temp_reg;
  int i;

  /* Compute frame_info.  */

  s390_frame_info (&frame);

  /* Choose best register to use for temp use within prologue.  */
  
  if (frame.return_reg_saved_p
      && !has_hard_reg_initial_val (Pmode, RETURN_REGNUM)
      && get_pool_size () < S390_POOL_CHUNK_MAX / 2)
    temp_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);
  else
    temp_reg = gen_rtx_REG (Pmode, 1);

  /* Save call saved gprs.  */

  if (frame.first_save_gpr != -1)
    {
      addr = plus_constant (stack_pointer_rtx, 
			    frame.first_save_gpr * UNITS_PER_WORD);
      addr = gen_rtx_MEM (Pmode, addr);
      set_mem_alias_set (addr, s390_sr_alias_set);

      if (frame.first_save_gpr != frame.last_save_gpr )
	{
	  insn = emit_insn (gen_store_multiple (addr,
			      gen_rtx_REG (Pmode, frame.first_save_gpr),
			      GEN_INT (frame.last_save_gpr 
				       - frame.first_save_gpr + 1)));

	  /* We need to set the FRAME_RELATED flag on all SETs
	     inside the store-multiple pattern.

	     However, we must not emit DWARF records for registers 2..5
	     if they are stored for use by variable arguments ...  

	     ??? Unfortunately, it is not enough to simply not the the
	     FRAME_RELATED flags for those SETs, because the first SET
	     of the PARALLEL is always treated as if it had the flag
	     set, even if it does not.  Therefore we emit a new pattern
	     without those registers as REG_FRAME_RELATED_EXPR note.  */

	  if (frame.first_save_gpr >= 6)
	    {
	      rtx pat = PATTERN (insn);

	      for (i = 0; i < XVECLEN (pat, 0); i++)
		if (GET_CODE (XVECEXP (pat, 0, i)) == SET)
		  RTX_FRAME_RELATED_P (XVECEXP (pat, 0, i)) = 1;

	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else if (frame.last_save_gpr >= 6)
	    {
	      rtx note, naddr;
	      naddr = plus_constant (stack_pointer_rtx, 6 * UNITS_PER_WORD);
	      note = gen_store_multiple (gen_rtx_MEM (Pmode, naddr), 
					 gen_rtx_REG (Pmode, 6),
					 GEN_INT (frame.last_save_gpr - 6 + 1));
	      REG_NOTES (insn) =
		gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, 
				   note, REG_NOTES (insn));

	      for (i = 0; i < XVECLEN (note, 0); i++)
		if (GET_CODE (XVECEXP (note, 0, i)) == SET)
		  RTX_FRAME_RELATED_P (XVECEXP (note, 0, i)) = 1;

	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
      else
	{
	  insn = emit_move_insn (addr, 
				 gen_rtx_REG (Pmode, frame.first_save_gpr));
          RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Dump constant pool and set constant pool register (13).  */
 
  insn = emit_insn (gen_lit ());
  
  /* Save fprs for variable args.  */

  if (current_function_stdarg || current_function_varargs)
    {
      /* Save fpr 0 and 2.  */ 

      save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 32, 16); 
      save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 24, 17); 
      
      if (TARGET_64BIT)
	{
	  /* Save fpr 4 and 6.  */
 
	  save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 16, 18); 
	  save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 8, 19); 
	}
    }

  /* Save fprs 4 and 6 if used (31 bit ABI).  */

  if (!TARGET_64BIT)
    {
      /* Save fpr 4 and 6.  */
      if (regs_ever_live[18])
	{
	  insn = save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 16, 18);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      if (regs_ever_live[19]) 
	{
	  insn = save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 8, 19); 
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Decrement stack pointer.  */

  if (frame.frame_size > 0)
    {
      rtx frame_off = GEN_INT (-frame.frame_size);

      /* Save incoming stack pointer into temp reg.  */
      
      if (TARGET_BACKCHAIN || frame.save_fprs_p)
	{
	  insn = emit_insn (gen_move_insn (temp_reg, stack_pointer_rtx));
	}
      
      /* Substract frame size from stack pointer.  */

      frame_off = GEN_INT (-frame.frame_size);
      if (!CONST_OK_FOR_LETTER_P (-frame.frame_size, 'K'))
	frame_off = force_const_mem (Pmode, frame_off);

      insn = emit_insn (gen_add2_insn (stack_pointer_rtx, frame_off));
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = 
	gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			   gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				   gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			           GEN_INT (-frame.frame_size))),
			   REG_NOTES (insn));

      /* Set backchain.  */
      
      if (TARGET_BACKCHAIN)
	{
	  addr = gen_rtx_MEM (Pmode, stack_pointer_rtx);
	  set_mem_alias_set (addr, s390_sr_alias_set);
	  insn = emit_insn (gen_move_insn (addr, temp_reg));
	}
    }

  /* Save fprs 8 - 15 (64 bit ABI).  */
  
  if (frame.save_fprs_p)
    {
      insn = emit_insn (gen_add2_insn (temp_reg, GEN_INT(-64)));

      for (i = 24; i < 32; i++)
	if (regs_ever_live[i])
	  {
	    rtx addr = plus_constant (stack_pointer_rtx, 
				      frame.frame_size - 64 + (i-24)*8);

	    insn = save_fpr (temp_reg, (i-24)*8, i);
	    RTX_FRAME_RELATED_P (insn) = 1;
	    REG_NOTES (insn) = 
	      gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode, 
			     gen_rtx_MEM (DFmode, addr),
			     gen_rtx_REG (DFmode, i)),
		REG_NOTES (insn));
	  }
    }
	    
  /* Set frame pointer, if needed.  */
  
  if (frame.frame_pointer_p)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Set up got pointer, if needed.  */
  
  if (current_function_uses_pic_offset_table)
    {
      rtx got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
      SYMBOL_REF_FLAG (got_symbol) = 1;

      if (TARGET_64BIT)
	{
	  insn = emit_insn (gen_movdi (pic_offset_table_rtx,
				       got_symbol));		 

          /* It can happen that the GOT pointer isn't really needed ...  */
          REG_NOTES(insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, NULL_RTX,
                                               REG_NOTES (insn));
	}
      else
	{
          got_symbol = gen_rtx_UNSPEC (VOIDmode, 
				       gen_rtvec (1, got_symbol), 100);
          got_symbol = gen_rtx_CONST (VOIDmode, got_symbol);
	  got_symbol = force_const_mem (Pmode, got_symbol);
	  insn = emit_move_insn (pic_offset_table_rtx,
				 got_symbol);		 
          REG_NOTES(insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, NULL_RTX,
                                               REG_NOTES (insn));

	  insn = emit_insn (gen_add2_insn (pic_offset_table_rtx,
					   gen_rtx_REG (Pmode, BASE_REGISTER)));
          REG_NOTES(insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, NULL_RTX,
                                               REG_NOTES (insn));
	}
    }      
}

/* Expand the epilogue into a bunch of separate insns.  */

void
s390_emit_epilogue ()
{
  struct s390_frame frame;
  rtx frame_pointer, return_reg;
  int area_bottom, area_top, offset;
  rtvec p;

  /* Compute frame_info.  */
 
  s390_frame_info (&frame);

  /* Check whether to use frame or stack pointer for restore.  */

  frame_pointer = frame.frame_pointer_p ? 
    hard_frame_pointer_rtx : stack_pointer_rtx;

  /* Compute which parts of the save area we need to access.  */

  if (frame.first_restore_gpr != -1)
    {
      area_bottom = frame.first_restore_gpr * UNITS_PER_WORD;
      area_top = (frame.last_save_gpr + 1) * UNITS_PER_WORD;
    }
  else
    {
      area_bottom = INT_MAX;
      area_top = INT_MIN;
    }

  if (TARGET_64BIT)
    {
      if (frame.save_fprs_p)
	{
	  if (area_bottom > -64)
	    area_bottom = -64;
	  if (area_top < 0)
	    area_top = 0;
	}
    }
  else
    {
      if (regs_ever_live[18])
	{
	  if (area_bottom > STACK_POINTER_OFFSET - 16)
	    area_bottom = STACK_POINTER_OFFSET - 16;
	  if (area_top < STACK_POINTER_OFFSET - 8)
	    area_top = STACK_POINTER_OFFSET - 8;
	}
      if (regs_ever_live[19])
	{
	  if (area_bottom > STACK_POINTER_OFFSET - 8)
	    area_bottom = STACK_POINTER_OFFSET - 8;
	  if (area_top < STACK_POINTER_OFFSET)
	    area_top = STACK_POINTER_OFFSET;
	}
    }

  /* Check whether we can access the register save area.  
     If not, increment the frame pointer as required.  */

  if (area_top <= area_bottom)
    {
      /* Nothing to restore.  */
    }
  else if (frame.frame_size + area_bottom >= 0
           && frame.frame_size + area_top <= 4096)
    {
      /* Area is in range.  */
      offset = frame.frame_size;
    }
  else
    {
      rtx insn, frame_off;

      offset = area_bottom < 0 ? -area_bottom : 0; 
      frame_off = GEN_INT (frame.frame_size - offset);

      if (!CONST_OK_FOR_LETTER_P (INTVAL (frame_off), 'K'))
	frame_off = force_const_mem (Pmode, frame_off);

      insn = emit_insn (gen_add2_insn (frame_pointer, frame_off));
    }

  /* Restore call saved fprs.  */

  if (TARGET_64BIT)
    {
      int i;

      if (frame.save_fprs_p)
	for (i = 24; i < 32; i++)
	  if (regs_ever_live[i] && !global_regs[i])
	    restore_fpr (frame_pointer, 
			 offset - 64 + (i-24) * 8, i);
    }
  else
    {
      if (regs_ever_live[18] && !global_regs[18])
	restore_fpr (frame_pointer, offset + STACK_POINTER_OFFSET - 16, 18);
      if (regs_ever_live[19] && !global_regs[19])
	restore_fpr (frame_pointer, offset + STACK_POINTER_OFFSET - 8, 19);
    }

  /* Return register.  */

  return_reg = gen_rtx_REG (Pmode, RETURN_REGNUM); 

  /* Restore call saved gprs.  */

  if (frame.first_restore_gpr != -1)
    {
      rtx addr;
      int i;

      /* Check for global register and save them 
	 to stack location from where they get restored.  */

      for (i = frame.first_restore_gpr; 
	   i <= frame.last_save_gpr;
	   i++)
	{
	  /* These registers are special and need to be 
	     restored in any case.  */
	  if (i == STACK_POINTER_REGNUM 
              || i == RETURN_REGNUM
              || i == BASE_REGISTER 
              || (flag_pic && i == PIC_OFFSET_TABLE_REGNUM))
	    continue;

	  if (global_regs[i])
	    {
	      addr = plus_constant (frame_pointer, 
		     offset + i * UNITS_PER_WORD);
	      addr = gen_rtx_MEM (Pmode, addr);
	      set_mem_alias_set (addr, s390_sr_alias_set);
	      emit_move_insn (addr, gen_rtx_REG (Pmode, i));
	    }  
	}

      /* Fetch return address from stack before load multiple,
	 this will do good for scheduling.  */

      if (frame.last_save_gpr >= RETURN_REGNUM 
	  && frame.first_restore_gpr < RETURN_REGNUM)
	{
	  int return_regnum = find_unused_clobbered_reg();
	  if (!return_regnum)
	    return_regnum = 4;
	  return_reg = gen_rtx_REG (Pmode, return_regnum);
	
	  addr = plus_constant (frame_pointer, 
				offset + RETURN_REGNUM * UNITS_PER_WORD); 
	  addr = gen_rtx_MEM (Pmode, addr);
	  set_mem_alias_set (addr, s390_sr_alias_set);
	  emit_move_insn (return_reg, addr);
	}

      /* ??? As references to the base register are not made
	 explicit in insn RTX code, we have to add a barrier here
	 to prevent incorrect scheduling.  */

      emit_insn (gen_blockage());      

      addr = plus_constant (frame_pointer, 
			    offset + frame.first_restore_gpr * UNITS_PER_WORD);
      addr = gen_rtx_MEM (Pmode, addr);
      set_mem_alias_set (addr, s390_sr_alias_set);

      if (frame.first_restore_gpr != frame.last_save_gpr)
	{
	  emit_insn (gen_load_multiple (
		       gen_rtx_REG (Pmode, frame.first_restore_gpr),
                       addr,
		       GEN_INT (frame.last_save_gpr - frame.first_restore_gpr + 1)));
	}
      else
	{
	  emit_move_insn (gen_rtx_REG (Pmode, frame.first_restore_gpr),
		 	  addr); 
	}
    }

  /* Return to caller.  */

  p = rtvec_alloc (2);
  
  RTVEC_ELT (p, 0) = gen_rtx_RETURN (VOIDmode);
  RTVEC_ELT (p, 1) = gen_rtx_USE (VOIDmode, return_reg);
  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));
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

  /* No type info available for some library calls ...  */
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

   The following global variables are used to initialize
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
			   GT, const1_rtx, Pmode, 0, lab_false);

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
      set_mem_alias_set (r, get_varargs_alias_set ());
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

/* Return rtx for 64-bit constant formed from the 32-bit subwords
   LOW and HIGH, independent of the host word size.  */

rtx
s390_gen_rtx_const_DI (high, low)
     int high;
     int low;
{
#if HOST_BITS_PER_WIDE_INT >= 64
  HOST_WIDE_INT val;
  val = (HOST_WIDE_INT)high;
  val <<= 32;
  val |= (HOST_WIDE_INT)low;
  
  return GEN_INT (val);
#else
#if HOST_BITS_PER_WIDE_INT >= 32
  return immed_double_const ((HOST_WIDE_INT)low, (HOST_WIDE_INT)high, DImode);
#else
  abort ();
#endif
#endif
} 

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

void
s390_function_profiler (file, labelno)
     FILE *file;
     int labelno;
{
  rtx op[7];

  char label[128];
  sprintf (label, "%sP%d", LPREFIX, labelno);

  fprintf (file, "# function profiler \n");

  op[0] = gen_rtx_REG (Pmode, RETURN_REGNUM);
  op[1] = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  op[1] = gen_rtx_MEM (Pmode, plus_constant (op[1], UNITS_PER_WORD));

  op[2] = gen_rtx_REG (Pmode, 1);
  op[3] = gen_rtx_SYMBOL_REF (Pmode, label);
  SYMBOL_REF_FLAG (op[3]) = 1;

  op[4] = gen_rtx_SYMBOL_REF (Pmode, "_mcount");
  if (flag_pic)
    {
      op[4] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op[4]), 113);
      op[4] = gen_rtx_CONST (Pmode, op[4]);
    }

  if (TARGET_64BIT)
    {
      output_asm_insn ("stg\t%0,%1", op);
      output_asm_insn ("larl\t%2,%3", op);
      output_asm_insn ("brasl\t%0,%4", op);
      output_asm_insn ("lg\t%0,%1", op);
    }
  else if (!flag_pic)
    {
      op[6] = gen_label_rtx ();

      output_asm_insn ("st\t%0,%1", op);
      output_asm_insn ("bras\t%2,%l6", op);
      output_asm_insn (".long\t%4", op);
      output_asm_insn (".long\t%3", op);
      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[6]));
      output_asm_insn ("l\t%0,0(%2)", op);
      output_asm_insn ("l\t%2,4(%2)", op);
      output_asm_insn ("basr\t%0,%0", op);
      output_asm_insn ("l\t%0,%1", op);
    }
  else
    {
      op[5] = gen_label_rtx ();
      op[6] = gen_label_rtx ();

      output_asm_insn ("st\t%0,%1", op);
      output_asm_insn ("bras\t%2,%l6", op);
      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[5]));
      output_asm_insn (".long\t%4-%l5", op);
      output_asm_insn (".long\t%3-%l5", op);
      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[6]));
      output_asm_insn ("lr\t%0,%2", op);
      output_asm_insn ("a\t%0,0(%2)", op);
      output_asm_insn ("a\t%2,4(%2)", op);
      output_asm_insn ("basr\t%0,%0", op);
      output_asm_insn ("l\t%0,%1", op);
    }
}

