/* Subroutines used for code generation on IBM S/390 and zSeries
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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
#include "langhooks.h"
#include "optabs.h"

static bool s390_assemble_integer PARAMS ((rtx, unsigned int, int));
static int s390_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int s390_adjust_priority PARAMS ((rtx, int));
static void s390_select_rtx_section PARAMS ((enum machine_mode, rtx, 
					     unsigned HOST_WIDE_INT));
static void s390_encode_section_info PARAMS ((tree, int));
static const char *s390_strip_name_encoding PARAMS ((const char *));
static bool s390_cannot_force_const_mem PARAMS ((rtx));
static void s390_init_builtins PARAMS ((void));
static rtx s390_expand_builtin PARAMS ((tree, rtx, rtx, 
					enum machine_mode, int));
static void s390_output_mi_thunk PARAMS ((FILE *, tree, HOST_WIDE_INT,
					  HOST_WIDE_INT, tree));

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER s390_assemble_integer

#undef  TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN ""

#undef  TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ""

#undef	TARGET_ASM_SELECT_RTX_SECTION
#define	TARGET_ASM_SELECT_RTX_SECTION  s390_select_rtx_section

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST s390_adjust_cost

#undef  TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY s390_adjust_priority

#undef	TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO s390_encode_section_info
#undef  TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING s390_strip_name_encoding

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif
#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM s390_cannot_force_const_mem

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS s390_init_builtins
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN s390_expand_builtin

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK s390_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_tree_hwi_hwi_tree_true

struct gcc_target targetm = TARGET_INITIALIZER;

extern int reload_completed;

/* The alias set for prologue/epilogue register save/restore.  */
static int s390_sr_alias_set = 0;

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */
rtx s390_compare_op0, s390_compare_op1;

/* The encoding characters for the four TLS models present in ELF.  */
static char const tls_model_chars[] = " GLil";

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

/* Define the structure for the machine field in struct function.  */

struct machine_function GTY(())
{
  /* Label of start of initial literal pool.  */
  rtx literal_pool_label;

  /* Set, if some of the fprs 8-15 need to be saved (64 bit abi).  */
  int save_fprs_p;

  /* Number of first and last gpr to be saved, restored.  */
  int first_save_gpr;
  int first_restore_gpr;
  int last_save_gpr;

  /* Size of stack frame.  */
  HOST_WIDE_INT frame_size;

  /* Some local-dynamic TLS symbol name.  */
  const char *some_ld_name;
};

static int s390_match_ccmode_set PARAMS ((rtx, enum machine_mode));
static int s390_branch_condition_mask PARAMS ((rtx));
static const char *s390_branch_condition_mnemonic PARAMS ((rtx, int));
static int check_mode PARAMS ((rtx, enum machine_mode *));
static int general_s_operand PARAMS ((rtx, enum machine_mode, int));
static int s390_decompose_address PARAMS ((rtx, struct s390_address *));
static rtx get_thread_pointer PARAMS ((void));
static rtx legitimize_tls_address PARAMS ((rtx, rtx));
static const char *get_some_local_dynamic_name PARAMS ((void));
static int get_some_local_dynamic_name_1 PARAMS ((rtx *, void *));
static int reg_used_in_mem_p PARAMS ((int, rtx));
static int addr_generation_dependency_p PARAMS ((rtx, rtx));
static int s390_split_branches PARAMS ((rtx, bool *));
static void find_constant_pool_ref PARAMS ((rtx, rtx *));
static void replace_constant_pool_ref PARAMS ((rtx *, rtx, rtx));
static int find_base_register_in_addr PARAMS ((struct s390_address *));
static bool find_base_register_ref PARAMS ((rtx));
static void replace_base_register_ref PARAMS ((rtx *, rtx));
static void s390_optimize_prolog PARAMS ((int));
static bool s390_fixup_clobbered_return_reg PARAMS ((rtx));
static int find_unused_clobbered_reg PARAMS ((void));
static void s390_frame_info PARAMS ((void));
static rtx save_fpr PARAMS ((rtx, int, int));
static rtx restore_fpr PARAMS ((rtx, int, int));
static rtx save_gprs PARAMS ((rtx, int, int, int));
static rtx restore_gprs PARAMS ((rtx, int, int, int));
static int s390_function_arg_size PARAMS ((enum machine_mode, tree));
static struct machine_function * s390_init_machine_status PARAMS ((void));
 
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
    case CCSRmode:
    case CCUmode:
    case CCURmode:
    case CCLmode:
    case CCL1mode:
    case CCL2mode:
    case CCT1mode:
    case CCT2mode:
    case CCT3mode:
      if (req_mode != set_mode)
        return 0;
      break;

    case CCZmode:
      if (req_mode != CCSmode && req_mode != CCUmode && req_mode != CCTmode
	  && req_mode != CCSRmode && req_mode != CCURmode)
        return 0;
      break;

    case CCAPmode:
    case CCANmode:
      if (req_mode != CCAmode)
        return 0;
      break;
 
    default:
      abort ();
    }
 
  return (GET_MODE (SET_SRC (set)) == set_mode);
}

/* Return true if every SET in INSN that sets the CC register 
   has source and destination with matching CC modes and that 
   CC mode is at least as constrained as REQ_MODE.  
   If REQ_MODE is VOIDmode, always return false.  */
 
int
s390_match_ccmode (insn, req_mode)
     rtx insn;
     enum machine_mode req_mode;
{
  int i;

  /* s390_tm_ccmode returns VOIDmode to indicate failure.  */
  if (req_mode == VOIDmode)
    return 0;

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

/* If a test-under-mask instruction can be used to implement 
   (compare (and ... OP1) OP2), return the CC mode required
   to do that.  Otherwise, return VOIDmode.  
   MIXED is true if the instruction can distinguish between
   CC1 and CC2 for mixed selected bits (TMxx), it is false
   if the instruction cannot (TM).  */

enum machine_mode
s390_tm_ccmode (op1, op2, mixed)
     rtx op1;
     rtx op2;
     int mixed;
{
  int bit0, bit1;

  /* ??? Fixme: should work on CONST_DOUBLE as well.  */
  if (GET_CODE (op1) != CONST_INT || GET_CODE (op2) != CONST_INT)
    return VOIDmode;

  /* Selected bits all zero: CC0.  */
  if (INTVAL (op2) == 0)
    return CCTmode;

  /* Selected bits all one: CC3.  */
  if (INTVAL (op2) == INTVAL (op1))
    return CCT3mode;

  /* Exactly two bits selected, mixed zeroes and ones: CC1 or CC2.  */
  if (mixed)
    {
      bit1 = exact_log2 (INTVAL (op2));
      bit0 = exact_log2 (INTVAL (op1) ^ INTVAL (op2));
      if (bit0 != -1 && bit1 != -1)
        return bit0 > bit1 ? CCT1mode : CCT2mode;
    }

  return VOIDmode;
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
	if (GET_CODE (op0) == PLUS && GET_CODE (XEXP (op0, 1)) == CONST_INT
	    && CONST_OK_FOR_LETTER_P (INTVAL (XEXP (op0, 1)), 'K')) 
	  return CCAPmode;
	if (GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS
	    || GET_CODE (op1) == NEG)
	  return CCLmode;

	if (GET_CODE (op0) == AND)
	  {
	    /* Check whether we can potentially do it via TM.  */
	    enum machine_mode ccmode;
	    ccmode = s390_tm_ccmode (XEXP (op0, 1), op1, 1);
	    if (ccmode != VOIDmode)
	      {
		/* Relax CCTmode to CCZmode to allow fall-back to AND
		   if that turns out to be beneficial.  */
	        return ccmode == CCTmode ? CCZmode : ccmode;
	      }
	  }

	if (register_operand (op0, HImode) 
	    && GET_CODE (op1) == CONST_INT
	    && (INTVAL (op1) == -1 || INTVAL (op1) == 65535))
	  return CCT3mode;
	if (register_operand (op0, QImode) 
	    && GET_CODE (op1) == CONST_INT
	    && (INTVAL (op1) == -1 || INTVAL (op1) == 255))
	  return CCT3mode;

	return CCZmode;

      case LE:
      case LT:
      case GE:
      case GT:
	  if (GET_CODE (op0) == PLUS && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (XEXP (op0, 1)), 'K')) 
            {
	      if (INTVAL (XEXP((op0), 1)) < 0)
	        return CCANmode;
              else
	        return CCAPmode;
	    }
      case UNORDERED:
      case ORDERED:
      case UNEQ:
      case UNLE:
      case UNLT:
      case UNGE:
      case UNGT:
      case LTGT:
	if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	    && GET_CODE (op1) != CONST_INT)
	  return CCSRmode;
	return CCSmode;

      case LTU:
      case GEU:
	if (GET_CODE (op0) == PLUS)
	  return CCL1mode;

	if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	    && GET_CODE (op1) != CONST_INT)
	  return CCURmode;
	return CCUmode;

      case LEU:
      case GTU:
	if (GET_CODE (op0) == MINUS)
	  return CCL2mode;

	if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	    && GET_CODE (op1) != CONST_INT)
	  return CCURmode;
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

    case CCT1mode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC1;
	case NE:	return CC0 | CC2 | CC3;
	default:
	  abort ();
        }
      break;

    case CCT2mode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC2;
	case NE:	return CC0 | CC1 | CC3;
	default:
	  abort ();
        }
      break;

    case CCT3mode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC3;
	case NE:	return CC0 | CC1 | CC2;
	default:
	  abort ();
        }
      break;

    case CCLmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0 | CC2;
	case NE:	return CC1 | CC3;
	default:
	  abort ();
        }
      break;

    case CCL1mode:
      switch (GET_CODE (code))
        {
	case LTU:	return CC2 | CC3;  /* carry */
	case GEU:	return CC0 | CC1;  /* no carry */
	default:
	  abort ();
        }
      break;

    case CCL2mode:
      switch (GET_CODE (code))
        {
	case GTU:	return CC0 | CC1;  /* borrow */
	case LEU:	return CC2 | CC3;  /* no borrow */
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

    case CCURmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
        case NE:	return CC2 | CC1 | CC3;
        case LTU:	return CC2;
        case GTU:	return CC1;
        case LEU:	return CC0 | CC2;
        case GEU:	return CC0 | CC1;
	default:
	  abort ();
        }
      break;

    case CCAPmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
        case NE:	return CC1 | CC2 | CC3;
        case LT:	return CC1 | CC3;
        case GT:	return CC2;
        case LE:	return CC0 | CC1 | CC3;
        case GE:	return CC0 | CC2;
	default:
	  abort ();
        }
      break;

    case CCANmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
        case NE:	return CC1 | CC2 | CC3;
        case LT:	return CC1;
        case GT:	return CC2 | CC3;
        case LE:	return CC0 | CC1;
        case GE:	return CC0 | CC2 | CC3;
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
      break;

    case CCSRmode:
      switch (GET_CODE (code))
        {
        case EQ:	return CC0;
        case NE:	return CC2 | CC1 | CC3;
        case LT:	return CC2;
        case GT:	return CC1;
        case LE:	return CC0 | CC2;
        case GE:	return CC0 | CC1;
	case UNORDERED:	return CC3;
	case ORDERED:	return CC0 | CC2 | CC1;
	case UNEQ:	return CC0 | CC3;
        case UNLT:	return CC2 | CC3;
        case UNGT:	return CC1 | CC3;
        case UNLE:	return CC0 | CC2 | CC3;
        case UNGE:	return CC0 | CC1 | CC3;
	case LTGT:	return CC2 | CC1;
	default:
	  abort ();
        }
      break;

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
  static const char *const mnemonic[16] =
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
      unsigned HOST_WIDE_INT value = 0;
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
      unsigned HOST_WIDE_INT value = 0;
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
      unsigned HOST_WIDE_INT value = 0;
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
      unsigned HOST_WIDE_INT value = 0;
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

/* Check whether we can (and want to) split a double-word 
   move in mode MODE from SRC to DST into two single-word 
   moves, moving the subword FIRST_SUBWORD first.  */

bool
s390_split_ok_p (dst, src, mode, first_subword)
     rtx dst;
     rtx src;
     enum machine_mode mode;
     int first_subword;
{
  /* Floating point registers cannot be split.  */
  if (FP_REG_P (src) || FP_REG_P (dst))
    return false;

  /* We don't need to split if operands are directly accessable.  */
  if (s_operand (src, mode) || s_operand (dst, mode))
    return false;

  /* Non-offsettable memory references cannot be split.  */
  if ((GET_CODE (src) == MEM && !offsettable_memref_p (src))
      || (GET_CODE (dst) == MEM && !offsettable_memref_p (dst)))
    return false;

  /* Moving the first subword must not clobber a register
     needed to move the second subword.  */
  if (register_operand (dst, mode))
    {
      rtx subreg = operand_subword (dst, first_subword, 0, mode);
      if (reg_overlap_mentioned_p (subreg, src))
        return false;
    }

  return true;
}


/* Change optimizations to be performed, depending on the 
   optimization level.

   LEVEL is the optimization level specified; 2 if `-O2' is
   specified, 1 if `-O' is specified, and 0 if neither is specified.

   SIZE is nonzero if `-Os' is specified and zero otherwise.  */

void
optimization_options (level, size)
     int level ATTRIBUTE_UNUSED;
     int size ATTRIBUTE_UNUSED;
{
  /* ??? There are apparently still problems with -fcaller-saves.  */
  flag_caller_saves = 0;

  /* By default, always emit DWARF-2 unwind info.  This allows debugging
     without maintaining a stack frame back-chain.  */
  flag_asynchronous_unwind_tables = 1;
}

void
override_options ()
{
  /* Acquire a unique set number for our register saves and restores.  */
  s390_sr_alias_set = new_alias_set ();

  /* Set up function hooks.  */
  init_machine_status = s390_init_machine_status;
}

/* Map for smallest class containing reg regno.  */

const enum reg_class regclass_map[FIRST_PSEUDO_REGISTER] =
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
      && XSTR (op, 0)[0] != '@'
      && !tls_symbolic_operand (op)
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
      && XSTR (op, 0)[0] != '@'
      && !tls_symbolic_operand (op)
      && (!flag_pic || SYMBOL_REF_FLAG (op)
          || CONSTANT_POOL_ADDRESS_P (op)))
    return 1;

  /* Now we must have a @GOTENT offset or @PLT stub
     or an @INDNTPOFF TLS offset.  */
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == 111)
    return 1;
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == 113)
    return 1;
  if (GET_CODE (op) == UNSPEC
      && XINT (op, 1) == UNSPEC_INDNTPOFF)
    return 1;

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
	if (s390_decompose_address (XEXP (op, 0), &addr) 
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

/* Return true if OP is a valid operand for a 'Q' constraint.
   This differs from s_operand in that only memory operands
   without index register are accepted, nothing else.  */

int
q_constraint (op)
     register rtx op;
{
  struct s390_address addr;

  if (GET_CODE (op) != MEM)
    return 0;

  if (!s390_decompose_address (XEXP (op, 0), &addr))
    return 0;

  if (addr.indx)
    return 0;

  return 1;
}

/* Return the cost of an address rtx ADDR.  */

int
s390_address_cost (addr)
     rtx addr;
{
  struct s390_address ad;
  if (!s390_decompose_address (addr, &ad))
    return 1000;

  return ad.indx? COSTS_N_INSNS (1) + 1 : COSTS_N_INSNS (1);
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

/* If OP is a SYMBOL_REF of a thread-local symbol, return its TLS mode,
   otherwise return 0.  */

int
tls_symbolic_operand (op)
     register rtx op;
{
  const char *symbol_str;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;
  symbol_str = XSTR (op, 0);

  if (symbol_str[0] != '%')
    return 0;
  return strchr (tls_model_chars, symbol_str[1]) - tls_model_chars;
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

/* Return true if OP contains a reference to a thread-local symbol.  */

int
tls_symbolic_reference_mentioned_p (op)
     rtx op;
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF)
    return tls_symbolic_operand (op);

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (tls_symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && tls_symbolic_reference_mentioned_p (XEXP (op, i)))
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
     via emit_symbolic_move.  */
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

  /* Accept immediate LARL operands.  */
  if (TARGET_64BIT && larl_operand (op, VOIDmode))
    return 1;

  /* Thread-local symbols are never legal constants.  This is
     so that emit_call knows that computing such addresses
     might require a function call.  */
  if (TLS_SYMBOLIC_CONST (op))
    return 0;

  /* In the PIC case, symbolic constants must *not* be
     forced into the literal pool.  We accept them here,
     so that they will be handled by emit_symbolic_move.  */
  if (flag_pic)
    return 1;

  /* All remaining non-PIC symbolic constants are
     forced into the literal pool.  */
  return 0;
}

/* Determine if it's legal to put X into the constant pool.  This
   is not possible if X contains the address of a symbol that is
   not constant (TLS) or not known at final link time (PIC).  */

static bool
s390_cannot_force_const_mem (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
      /* Accept all non-symbolic constants.  */
      return false;

    case LABEL_REF:
      /* Labels are OK iff we are non-PIC.  */
      return flag_pic != 0;

    case SYMBOL_REF:
      /* 'Naked' TLS symbol references are never OK,
         non-TLS symbols are OK iff we are non-PIC.  */
      if (tls_symbolic_operand (x))
	return true;
      else
	return flag_pic != 0;

    case CONST:
      return s390_cannot_force_const_mem (XEXP (x, 0));
    case PLUS:
    case MINUS:
      return s390_cannot_force_const_mem (XEXP (x, 0))
	     || s390_cannot_force_const_mem (XEXP (x, 1));

    case UNSPEC:
      switch (XINT (x, 1))
	{
	/* Only lt-relative or GOT-relative UNSPECs are OK.  */
	case 100:
	case 104:
	case 112:
	case 114:
	case UNSPEC_TLSGD:
	case UNSPEC_TLSLDM:
	case UNSPEC_NTPOFF:
	case UNSPEC_DTPOFF:
	case UNSPEC_GOTNTPOFF:
	case UNSPEC_INDNTPOFF:
	  return false;

	default:
	  return true;
	}
      break;

    default:
      abort ();
    }
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
	 literal pool.  */

      case CONST_DOUBLE:
      case CONST_INT:
	if (legitimate_reload_constant_p (op))
	  return class;
	else
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

/* Return the register class of a scratch register needed to
   store a register of class CLASS in MODE into OUT:

   We need a temporary when storing a double-word to a 
   non-offsettable memory address.  */

enum reg_class
s390_secondary_output_reload_class (class, mode, out)
     enum reg_class class;
     enum machine_mode mode;
     rtx out;
{
  if ((TARGET_64BIT ? mode == TImode
                    : (mode == DImode || mode == DFmode))
      && reg_classes_intersect_p (GENERAL_REGS, class)
      && GET_CODE (out) == MEM
      && !offsettable_memref_p (out)
      && !s_operand (out, VOIDmode))
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
s390_expand_plus_operand (target, src, scratch)
     register rtx target;
     register rtx src;
     register rtx scratch;
{
  rtx sum1, sum2;
  struct s390_address ad;

  /* src must be a PLUS; get its two operands.  */
  if (GET_CODE (src) != PLUS || GET_MODE (src) != Pmode)
    abort ();

  /* Check if any of the two operands is already scheduled
     for replacement by reload.  This can happen e.g. when
     float registers occur in an address.  */
  sum1 = find_replacement (&XEXP (src, 0));
  sum2 = find_replacement (&XEXP (src, 1));
  src = gen_rtx_PLUS (Pmode, sum1, sum2);

  /* If the address is already strictly valid, there's nothing to do.  */
  if (!s390_decompose_address (src, &ad)
      || (ad.base && !REG_OK_FOR_BASE_STRICT_P (ad.base))
      || (ad.indx && !REG_OK_FOR_INDEX_STRICT_P (ad.indx)))
    {
      /* Otherwise, one of the operands cannot be an address register;
         we reload its value into the scratch register.  */
      if (true_regnum (sum1) < 1 || true_regnum (sum1) > 15)
	{
	  emit_move_insn (scratch, sum1);
	  sum1 = scratch;
	}
      if (true_regnum (sum2) < 1 || true_regnum (sum2) > 15)
	{
	  emit_move_insn (scratch, sum2);
	  sum2 = scratch;
	}

      /* According to the way these invalid addresses are generated
         in reload.c, it should never happen (at least on s390) that
         *neither* of the PLUS components, after find_replacements
         was applied, is an address register.  */
      if (sum1 == scratch && sum2 == scratch)
	{
	  debug_rtx (src);
	  abort ();
	}

      src = gen_rtx_PLUS (Pmode, sum1, sum2);
    }

  /* Emit the LOAD ADDRESS pattern.  Note that reload of PLUS
     is only ever performed on addresses, so we can mark the
     sum as legitimate for LA in any case.  */
  s390_load_address (target, src);
}


/* Decompose a RTL expression ADDR for a memory address into
   its components, returned in OUT.

   Returns 0 if ADDR is not a valid memory address, nonzero
   otherwise.  If OUT is NULL, don't return the components,
   but check for validity only.

   Note: Only addresses in canonical form are recognized.
   LEGITIMIZE_ADDRESS should convert non-canonical forms to the
   canonical form so that they will be recognized.  */

static int
s390_decompose_address (addr, out)
     register rtx addr;
     struct s390_address *out;
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


  /* Prefer to use pointer as base, not index.  */
  if (base && indx)
    {
      int base_ptr = GET_CODE (base) == UNSPEC
		     || (REG_P (base) && REG_POINTER (base));
      int indx_ptr = GET_CODE (indx) == UNSPEC
		     || (REG_P (indx) && REG_POINTER (indx));

      if (!base_ptr && indx_ptr)
	{
	  rtx tmp = base;
	  base = indx;
	  indx = tmp;
	}
    }

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

      if (REGNO (base) == BASE_REGISTER
	  || REGNO (base) == STACK_POINTER_REGNUM
	  || REGNO (base) == FRAME_POINTER_REGNUM
	  || ((reload_completed || reload_in_progress)
	      && frame_pointer_needed
	      && REGNO (base) == HARD_FRAME_POINTER_REGNUM)
	  || REGNO (base) == ARG_POINTER_REGNUM
	  || (REGNO (base) >= FIRST_VIRTUAL_REGISTER
	      && REGNO (base) <= LAST_VIRTUAL_REGISTER)
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

      if (REGNO (indx) == BASE_REGISTER
	  || REGNO (indx) == STACK_POINTER_REGNUM
	  || REGNO (indx) == FRAME_POINTER_REGNUM
	  || ((reload_completed || reload_in_progress)
	      && frame_pointer_needed
	      && REGNO (indx) == HARD_FRAME_POINTER_REGNUM)
	  || REGNO (indx) == ARG_POINTER_REGNUM
	  || (REGNO (indx) >= FIRST_VIRTUAL_REGISTER
	      && REGNO (indx) <= LAST_VIRTUAL_REGISTER)
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
	  /* If the argument pointer is involved, the displacement will change
	     later anyway as the argument pointer gets eliminated.  This could
	     make a valid displacement invalid, but it is more likely to make
	     an invalid displacement valid, because we sometimes access the
	     register save area via negative offsets to the arg pointer.
	     Thus we don't check the displacement for validity here.  If after
	     elimination the displacement turns out to be invalid after all,
	     this is fixed up by reload in any case.  */
	  if (base != arg_pointer_rtx && indx != arg_pointer_rtx)
	    {
	      if (INTVAL (disp) < 0 || INTVAL (disp) >= 4096)
	        return FALSE;
	    }
        }

      /* In the small-PIC case, the linker converts @GOT12 
         and @GOTNTPOFF offsets to possible displacements.  */
      else if (GET_CODE (disp) == CONST
               && GET_CODE (XEXP (disp, 0)) == UNSPEC
               && (XINT (XEXP (disp, 0), 1) == 110
		   || XINT (XEXP (disp, 0), 1) == UNSPEC_GOTNTPOFF))
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
  struct s390_address ad;
  if (!s390_decompose_address (addr, &ad))
    return FALSE;

  if (strict)
    {
      if (ad.base && !REG_OK_FOR_BASE_STRICT_P (ad.base))
	return FALSE;
      if (ad.indx && !REG_OK_FOR_INDEX_STRICT_P (ad.indx))
	return FALSE;
    }
  else
    {
      if (ad.base && !REG_OK_FOR_BASE_NONSTRICT_P (ad.base))
	return FALSE;
      if (ad.indx && !REG_OK_FOR_INDEX_NONSTRICT_P (ad.indx))
	return FALSE;
    }

  return TRUE;
}

/* Return 1 if OP is a valid operand for the LA instruction.
   In 31-bit, we need to prove that the result is used as an
   address, as LA performs only a 31-bit addition.  */

int
legitimate_la_operand_p (op)
     register rtx op;
{
  struct s390_address addr;
  if (!s390_decompose_address (op, &addr))
    return FALSE;

  if (TARGET_64BIT || addr.pointer)
    return TRUE;

  return FALSE;
}

/* Return 1 if OP is a valid operand for the LA instruction,
   and we prefer to use LA over addition to compute it.  */
   
int
preferred_la_operand_p (op)
     register rtx op;
{
  struct s390_address addr;
  if (!s390_decompose_address (op, &addr))
    return FALSE;

  if (!TARGET_64BIT && !addr.pointer)
    return FALSE;

  if (addr.pointer)
    return TRUE;

  if ((addr.base && REG_P (addr.base) && REG_POINTER (addr.base))
      || (addr.indx && REG_P (addr.indx) && REG_POINTER (addr.indx)))
    return TRUE;

  return FALSE;
}

/* Emit a forced load-address operation to load SRC into DST.
   This will use the LOAD ADDRESS instruction even in situations
   where legitimate_la_operand_p (SRC) returns false.  */

void
s390_load_address (dst, src)
     rtx dst;
     rtx src;
{
  if (TARGET_64BIT)
    emit_move_insn (dst, src);
  else
    emit_insn (gen_force_la_31 (dst, src));
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
      if (TARGET_64BIT && larl_operand (addr, VOIDmode))
        {
          /* Access local symbols PC-relative via LARL.  
             This is the same as in the non-PIC case, so it is 
             handled automatically ...  */
        }
      else
        {
          /* Access local symbols relative to the literal pool.  */

          rtx temp = reg? reg : gen_reg_rtx (Pmode);

          addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), 100);
          addr = gen_rtx_CONST (Pmode, addr);
          addr = force_const_mem (Pmode, addr);
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

	  if (reload_in_progress || reload_completed)
	    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;

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

	  if (reload_in_progress || reload_completed)
	    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;

          addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), 112);
          addr = gen_rtx_CONST (Pmode, addr);
          addr = force_const_mem (Pmode, addr);
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
                    new = force_const_mem (Pmode, orig);
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
                        addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), 114);
                        addr = gen_rtx_CONST (Pmode, addr);
                        addr = force_const_mem (Pmode, addr);
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
              if (TARGET_64BIT && larl_operand (op0, VOIDmode))
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

                  addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0), 100);
                  addr = gen_rtx_PLUS (Pmode, addr, op1);
                  addr = gen_rtx_CONST (Pmode, addr);
                  addr = force_const_mem (Pmode, addr);
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
	           && GET_CODE (op1) == CONST_INT
	           && XINT (op0, 1) == 100)
            {
	      if (XVECLEN (op0, 0) != 1)
                abort ();

              new = force_const_mem (Pmode, orig);
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

/* Load the thread pointer into a register.  */

static rtx
get_thread_pointer ()
{
  rtx tp;

  tp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TP);
  tp = force_reg (Pmode, tp);
  mark_reg_pointer (tp, BITS_PER_WORD);

  return tp;
}

/* Construct the SYMBOL_REF for the tls_get_offset function.  */

static GTY(()) rtx s390_tls_symbol;
rtx
s390_tls_get_offset ()
{
  if (!s390_tls_symbol)
    s390_tls_symbol = gen_rtx_SYMBOL_REF (Pmode, "__tls_get_offset");

  return s390_tls_symbol;
}

/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  REG may be used as temporary.  */

static rtx
legitimize_tls_address (addr, reg)
     rtx addr;
     rtx reg;
{
  rtx new, tls_call, temp, base, r2, insn;

  if (GET_CODE (addr) == SYMBOL_REF)
    switch (tls_symbolic_operand (addr))
      {
      case TLS_MODEL_GLOBAL_DYNAMIC:
	start_sequence ();
	r2 = gen_rtx_REG (Pmode, 2);
	tls_call = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_TLSGD);
	new = gen_rtx_CONST (Pmode, tls_call);
	new = force_const_mem (Pmode, new);
	emit_move_insn (r2, new);
	emit_call_insn (gen_call_value_tls (r2, tls_call));
	insn = get_insns ();
	end_sequence ();

	new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_NTPOFF);
	temp = gen_reg_rtx (Pmode);
	emit_libcall_block (insn, temp, r2, new);

	new = gen_rtx_PLUS (Pmode, get_thread_pointer (), temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new);
	    new = reg;
	  }
	break;

      case TLS_MODEL_LOCAL_DYNAMIC:
	start_sequence ();
	r2 = gen_rtx_REG (Pmode, 2);
	tls_call = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TLSLDM);
	new = gen_rtx_CONST (Pmode, tls_call);
	new = force_const_mem (Pmode, new);
	emit_move_insn (r2, new);
	emit_call_insn (gen_call_value_tls (r2, tls_call));
	insn = get_insns ();
	end_sequence ();

	new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TLSLDM_NTPOFF);
	temp = gen_reg_rtx (Pmode);
	emit_libcall_block (insn, temp, r2, new);

	new = gen_rtx_PLUS (Pmode, get_thread_pointer (), temp);
	base = gen_reg_rtx (Pmode);
	s390_load_address (base, new);

	new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_DTPOFF);
	new = gen_rtx_CONST (Pmode, new);
	new = force_const_mem (Pmode, new);
	temp = gen_reg_rtx (Pmode);
	emit_move_insn (temp, new);

	new = gen_rtx_PLUS (Pmode, base, temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new);
	    new = reg;
	  }
	break;

      case TLS_MODEL_INITIAL_EXEC:
	if (flag_pic == 1)
	  {
	    /* Assume GOT offset < 4k.  This is handled the same way
	       in both 31- and 64-bit code.  */

	    if (reload_in_progress || reload_completed)
	      regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;

	    new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTNTPOFF);
	    new = gen_rtx_CONST (Pmode, new);
	    new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);
	    new = gen_rtx_MEM (Pmode, new);
	    RTX_UNCHANGING_P (new) = 1;
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new);
	  }
	else if (TARGET_64BIT)
	  {
	    /* If the GOT offset might be >= 4k, we determine the position
	       of the GOT entry via a PC-relative LARL.  */

	    new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_INDNTPOFF);
	    new = gen_rtx_CONST (Pmode, new);
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new);

	    new = gen_rtx_MEM (Pmode, temp);
	    RTX_UNCHANGING_P (new) = 1;
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new);
	  }
	else if (flag_pic)
	  {
	    /* If the GOT offset might be >= 4k, we have to load it 
	       from the literal pool.  */

	    if (reload_in_progress || reload_completed)
	      regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;

	    new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTNTPOFF);
	    new = gen_rtx_CONST (Pmode, new);
	    new = force_const_mem (Pmode, new);
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new);

            new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, temp);
	    new = gen_rtx_MEM (Pmode, new);
	    RTX_UNCHANGING_P (new) = 1;

	    new = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, new, addr), UNSPEC_TLS_LOAD);
	    temp = gen_reg_rtx (Pmode);
	    emit_insn (gen_rtx_SET (Pmode, temp, new));
	  }
	else
	  {
	    /* In position-dependent code, load the absolute address of
	       the GOT entry from the literal pool.  */

	    new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_INDNTPOFF);
	    new = gen_rtx_CONST (Pmode, new);
	    new = force_const_mem (Pmode, new);
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new);

	    new = temp;
	    new = gen_rtx_MEM (Pmode, new);
	    RTX_UNCHANGING_P (new) = 1;

	    new = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, new, addr), UNSPEC_TLS_LOAD);
	    temp = gen_reg_rtx (Pmode);
	    emit_insn (gen_rtx_SET (Pmode, temp, new));
	  }

	new = gen_rtx_PLUS (Pmode, get_thread_pointer (), temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new);
	    new = reg;
	  }
	break;

      case TLS_MODEL_LOCAL_EXEC:
	new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_NTPOFF);
	new = gen_rtx_CONST (Pmode, new);
	new = force_const_mem (Pmode, new);
        temp = gen_reg_rtx (Pmode);
	emit_move_insn (temp, new);

	new = gen_rtx_PLUS (Pmode, get_thread_pointer (), temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new);
	    new = reg;
	  }
	break;

      default:
	abort ();
      }

  else if (GET_CODE (addr) == CONST && GET_CODE (XEXP (addr, 0)) == UNSPEC)
    {
      switch (XINT (XEXP (addr, 0), 1))
	{
	case UNSPEC_INDNTPOFF:
	  if (TARGET_64BIT)
	    new = addr;
	  else
	    abort ();
	  break;

	default:
	  abort ();
	}
    }

  else
    abort ();  /* for now ... */

  return new;
}

/* Emit insns to move operands[1] into operands[0].  */

void
emit_symbolic_move (operands)
     rtx *operands;
{
  rtx temp = no_new_pseudos ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (Pmode, operands[1]);
  else if (TLS_SYMBOLIC_CONST (operands[1]))
    operands[1] = legitimize_tls_address (operands[1], temp);
  else if (flag_pic)
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

  if (TLS_SYMBOLIC_CONST (x))
    {
      x = legitimize_tls_address (x, 0);

      if (legitimate_address_p (mode, x, FALSE))
	return x;
    }
  else if (flag_pic)
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

  /* Optimize loading of large displacements by splitting them
     into the multiple of 4K and the rest; this allows the
     former to be CSE'd if possible. 

     Don't do this if the displacement is added to a register
     pointing into the stack frame, as the offsets will
     change later anyway.  */

  if (GET_CODE (constant_term) == CONST_INT
      && (INTVAL (constant_term) < 0
          || INTVAL (constant_term) >= 4096)
      && !(REG_P (x) && REGNO_PTR_FRAME_P (REGNO (x))))
    {
      HOST_WIDE_INT lower = INTVAL (constant_term) & 0xfff;
      HOST_WIDE_INT upper = INTVAL (constant_term) ^ lower;

      rtx temp = gen_reg_rtx (Pmode);
      rtx val  = force_operand (GEN_INT (upper), temp);
      if (val != temp)
	emit_move_insn (temp, val);

      x = gen_rtx_PLUS (Pmode, x, temp);
      constant_term = GEN_INT (lower);
    }

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

/* Emit code to move LEN bytes from DST to SRC.  */

void
s390_expand_movstr (dst, src, len)
     rtx dst;
     rtx src;
     rtx len;
{
  rtx (*gen_short) PARAMS ((rtx, rtx, rtx)) = 
    TARGET_64BIT ? gen_movstr_short_64 : gen_movstr_short_31;
  rtx (*gen_long) PARAMS ((rtx, rtx, rtx, rtx)) = 
    TARGET_64BIT ? gen_movstr_long_64 : gen_movstr_long_31;


  if (GET_CODE (len) == CONST_INT && INTVAL (len) >= 0 && INTVAL (len) <= 256)
    {
      if (INTVAL (len) > 0)
        emit_insn ((*gen_short) (dst, src, GEN_INT (INTVAL (len) - 1)));
    }

  else if (TARGET_MVCLE)
    {
      enum machine_mode double_mode = TARGET_64BIT ? TImode : DImode;
      enum machine_mode single_mode = TARGET_64BIT ? DImode : SImode;
      rtx reg0 = gen_reg_rtx (double_mode);
      rtx reg1 = gen_reg_rtx (double_mode);

      emit_insn (gen_rtx_CLOBBER (VOIDmode, reg0));
      emit_insn (gen_rtx_CLOBBER (VOIDmode, reg1));

      emit_move_insn (gen_highpart (single_mode, reg0), 
		      force_operand (XEXP (dst, 0), NULL_RTX));
      emit_move_insn (gen_highpart (single_mode, reg1), 
		      force_operand (XEXP (src, 0), NULL_RTX));

      convert_move (gen_lowpart (single_mode, reg0), len, 1);
      convert_move (gen_lowpart (single_mode, reg1), len, 1);

      emit_insn ((*gen_long) (reg0, reg1, reg0, reg1));
    }

  else
    {
      rtx dst_addr, src_addr, count, blocks, temp;
      rtx end_label = gen_label_rtx ();
      enum machine_mode mode;
      tree type;

      mode = GET_MODE (len);
      if (mode == VOIDmode)
        mode = word_mode;

      type = (*lang_hooks.types.type_for_mode) (mode, 1);
      if (!type)
        abort ();

      dst_addr = gen_reg_rtx (Pmode);
      src_addr = gen_reg_rtx (Pmode);
      count = gen_reg_rtx (mode);
      blocks = gen_reg_rtx (mode);

      convert_move (count, len, 1);
      emit_cmp_and_jump_insns (count, const0_rtx, 
			       EQ, NULL_RTX, mode, 1, end_label);

      emit_move_insn (dst_addr, force_operand (XEXP (dst, 0), NULL_RTX));
      emit_move_insn (src_addr, force_operand (XEXP (src, 0), NULL_RTX));
      dst = change_address (dst, VOIDmode, dst_addr);
      src = change_address (src, VOIDmode, src_addr);
     
      temp = expand_binop (mode, add_optab, count, constm1_rtx, count, 1, 0);
      if (temp != count)
        emit_move_insn (count, temp);

      temp = expand_binop (mode, ashr_optab, count, GEN_INT (8), blocks, 1, 0);
      if (temp != blocks)
        emit_move_insn (blocks, temp);

      expand_start_loop (1);
      expand_exit_loop_top_cond (0, build (NE_EXPR, type,
					   make_tree (type, blocks),
					   make_tree (type, const0_rtx)));

      emit_insn ((*gen_short) (dst, src, GEN_INT (255)));
      s390_load_address (dst_addr, 
			 gen_rtx_PLUS (Pmode, dst_addr, GEN_INT (256)));
      s390_load_address (src_addr, 
			 gen_rtx_PLUS (Pmode, src_addr, GEN_INT (256)));
      
      temp = expand_binop (mode, add_optab, blocks, constm1_rtx, blocks, 1, 0);
      if (temp != blocks)
        emit_move_insn (blocks, temp);

      expand_end_loop ();

      emit_insn ((*gen_short) (dst, src, convert_to_mode (word_mode, count, 1)));
      emit_label (end_label);
    }
}

/* Emit code to clear LEN bytes at DST.  */

void
s390_expand_clrstr (dst, len)
     rtx dst;
     rtx len;
{
  rtx (*gen_short) PARAMS ((rtx, rtx)) = 
    TARGET_64BIT ? gen_clrstr_short_64 : gen_clrstr_short_31;
  rtx (*gen_long) PARAMS ((rtx, rtx, rtx)) = 
    TARGET_64BIT ? gen_clrstr_long_64 : gen_clrstr_long_31;


  if (GET_CODE (len) == CONST_INT && INTVAL (len) >= 0 && INTVAL (len) <= 256)
    {
      if (INTVAL (len) > 0)
        emit_insn ((*gen_short) (dst, GEN_INT (INTVAL (len) - 1)));
    }

  else if (TARGET_MVCLE)
    {
      enum machine_mode double_mode = TARGET_64BIT ? TImode : DImode;
      enum machine_mode single_mode = TARGET_64BIT ? DImode : SImode;
      rtx reg0 = gen_reg_rtx (double_mode);
      rtx reg1 = gen_reg_rtx (double_mode);

      emit_insn (gen_rtx_CLOBBER (VOIDmode, reg0));
      emit_insn (gen_rtx_CLOBBER (VOIDmode, reg1));

      emit_move_insn (gen_highpart (single_mode, reg0), 
		      force_operand (XEXP (dst, 0), NULL_RTX));
      convert_move (gen_lowpart (single_mode, reg0), len, 1);

      emit_move_insn (gen_highpart (single_mode, reg1), const0_rtx);
      emit_move_insn (gen_lowpart (single_mode, reg1), const0_rtx);

      emit_insn ((*gen_long) (reg0, reg1, reg0));
    }

  else
    {
      rtx dst_addr, src_addr, count, blocks, temp;
      rtx end_label = gen_label_rtx ();
      enum machine_mode mode;
      tree type;

      mode = GET_MODE (len);
      if (mode == VOIDmode)
        mode = word_mode;

      type = (*lang_hooks.types.type_for_mode) (mode, 1);
      if (!type)
        abort ();

      dst_addr = gen_reg_rtx (Pmode);
      src_addr = gen_reg_rtx (Pmode);
      count = gen_reg_rtx (mode);
      blocks = gen_reg_rtx (mode);

      convert_move (count, len, 1);
      emit_cmp_and_jump_insns (count, const0_rtx, 
			       EQ, NULL_RTX, mode, 1, end_label);

      emit_move_insn (dst_addr, force_operand (XEXP (dst, 0), NULL_RTX));
      dst = change_address (dst, VOIDmode, dst_addr);
     
      temp = expand_binop (mode, add_optab, count, constm1_rtx, count, 1, 0);
      if (temp != count)
        emit_move_insn (count, temp);

      temp = expand_binop (mode, ashr_optab, count, GEN_INT (8), blocks, 1, 0);
      if (temp != blocks)
        emit_move_insn (blocks, temp);

      expand_start_loop (1);
      expand_exit_loop_top_cond (0, build (NE_EXPR, type,
					   make_tree (type, blocks),
					   make_tree (type, const0_rtx)));

      emit_insn ((*gen_short) (dst, GEN_INT (255)));
      s390_load_address (dst_addr, 
			 gen_rtx_PLUS (Pmode, dst_addr, GEN_INT (256)));
      
      temp = expand_binop (mode, add_optab, blocks, constm1_rtx, blocks, 1, 0);
      if (temp != blocks)
        emit_move_insn (blocks, temp);

      expand_end_loop ();

      emit_insn ((*gen_short) (dst, convert_to_mode (word_mode, count, 1)));
      emit_label (end_label);
    }
}

/* Emit code to compare LEN bytes at OP0 with those at OP1,
   and return the result in TARGET.  */

void
s390_expand_cmpmem (target, op0, op1, len)
     rtx target;
     rtx op0;
     rtx op1;
     rtx len;
{
  rtx (*gen_short) PARAMS ((rtx, rtx, rtx)) = 
    TARGET_64BIT ? gen_cmpmem_short_64 : gen_cmpmem_short_31;
  rtx (*gen_long) PARAMS ((rtx, rtx, rtx, rtx)) = 
    TARGET_64BIT ? gen_cmpmem_long_64 : gen_cmpmem_long_31;
  rtx (*gen_result) PARAMS ((rtx)) =
    GET_MODE (target) == DImode ? gen_cmpint_di : gen_cmpint_si;

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);
  len = protect_from_queue (len, 0);

  if (GET_CODE (len) == CONST_INT && INTVAL (len) >= 0 && INTVAL (len) <= 256)
    {
      if (INTVAL (len) > 0)
        {
          emit_insn ((*gen_short) (op0, op1, GEN_INT (INTVAL (len) - 1)));
          emit_insn ((*gen_result) (target));
        }
      else
        emit_move_insn (target, const0_rtx);
    }

  else /* if (TARGET_MVCLE) */
    {
      enum machine_mode double_mode = TARGET_64BIT ? TImode : DImode;
      enum machine_mode single_mode = TARGET_64BIT ? DImode : SImode;
      rtx reg0 = gen_reg_rtx (double_mode);
      rtx reg1 = gen_reg_rtx (double_mode);

      emit_insn (gen_rtx_CLOBBER (VOIDmode, reg0));
      emit_insn (gen_rtx_CLOBBER (VOIDmode, reg1));

      emit_move_insn (gen_highpart (single_mode, reg0), 
		      force_operand (XEXP (op0, 0), NULL_RTX));
      emit_move_insn (gen_highpart (single_mode, reg1), 
		      force_operand (XEXP (op1, 0), NULL_RTX));

      convert_move (gen_lowpart (single_mode, reg0), len, 1);
      convert_move (gen_lowpart (single_mode, reg1), len, 1);

      emit_insn ((*gen_long) (reg0, reg1, reg0, reg1));
      emit_insn ((*gen_result) (target));
    }

#if 0
  /* Deactivate for now as profile code cannot cope with
     CC being live across basic block boundaries.  */
  else
    {
      rtx addr0, addr1, count, blocks, temp;
      rtx end_label = gen_label_rtx ();
      enum machine_mode mode;
      tree type;

      mode = GET_MODE (len);
      if (mode == VOIDmode)
        mode = word_mode;

      type = (*lang_hooks.types.type_for_mode) (mode, 1);
      if (!type)
        abort ();

      addr0 = gen_reg_rtx (Pmode);
      addr1 = gen_reg_rtx (Pmode);
      count = gen_reg_rtx (mode);
      blocks = gen_reg_rtx (mode);

      convert_move (count, len, 1);
      emit_cmp_and_jump_insns (count, const0_rtx, 
			       EQ, NULL_RTX, mode, 1, end_label);

      emit_move_insn (addr0, force_operand (XEXP (op0, 0), NULL_RTX));
      emit_move_insn (addr1, force_operand (XEXP (op1, 0), NULL_RTX));
      op0 = change_address (op0, VOIDmode, addr0);
      op1 = change_address (op1, VOIDmode, addr1);
     
      temp = expand_binop (mode, add_optab, count, constm1_rtx, count, 1, 0);
      if (temp != count)
        emit_move_insn (count, temp);

      temp = expand_binop (mode, ashr_optab, count, GEN_INT (8), blocks, 1, 0);
      if (temp != blocks)
        emit_move_insn (blocks, temp);

      expand_start_loop (1);
      expand_exit_loop_top_cond (0, build (NE_EXPR, type,
					   make_tree (type, blocks),
					   make_tree (type, const0_rtx)));

      emit_insn ((*gen_short) (op0, op1, GEN_INT (255)));
      temp = gen_rtx_NE (VOIDmode, gen_rtx_REG (CCSmode, 33), const0_rtx);
      temp = gen_rtx_IF_THEN_ELSE (VOIDmode, temp, 
			gen_rtx_LABEL_REF (VOIDmode, end_label), pc_rtx);
      temp = gen_rtx_SET (VOIDmode, pc_rtx, temp);
      emit_jump_insn (temp);

      s390_load_address (addr0, 
			 gen_rtx_PLUS (Pmode, addr0, GEN_INT (256)));
      s390_load_address (addr1, 
			 gen_rtx_PLUS (Pmode, addr1, GEN_INT (256)));
      
      temp = expand_binop (mode, add_optab, blocks, constm1_rtx, blocks, 1, 0);
      if (temp != blocks)
        emit_move_insn (blocks, temp);

      expand_end_loop ();

      emit_insn ((*gen_short) (op0, op1, convert_to_mode (word_mode, count, 1)));
      emit_label (end_label);

      emit_insn ((*gen_result) (target));
    }
#endif
}

/* This is called from dwarf2out.c via ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

void
s390_output_dwarf_dtprel (file, size, x)
     FILE *file;
     int size;
     rtx x;
{
  switch (size)
    {
    case 4:
      fputs ("\t.long\t", file);
      break;
    case 8:
      fputs ("\t.quad\t", file);
      break;
    default:
      abort ();
    }
  output_addr_const (file, x);
  fputs ("@DTPOFF", file);
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

/* Locate some local-dynamic symbol still in use by this function
   so that we can print its name in local-dynamic base patterns.  */

static const char *
get_some_local_dynamic_name ()
{
  rtx insn;

  if (cfun->machine->some_ld_name)
    return cfun->machine->some_ld_name;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
        && for_each_rtx (&PATTERN (insn), get_some_local_dynamic_name_1, 0))
      return cfun->machine->some_ld_name;

  abort ();
}

static int
get_some_local_dynamic_name_1 (px, data)
     rtx *px;
     void *data ATTRIBUTE_UNUSED;
{
  rtx x = *px;

  if (GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x))
    {
      x = get_pool_constant (x);
      return for_each_rtx (&x, get_some_local_dynamic_name_1, 0);
    }

  if (GET_CODE (x) == SYMBOL_REF
      && tls_symbolic_operand (x) == TLS_MODEL_LOCAL_DYNAMIC)
    {
      cfun->machine->some_ld_name = XSTR (x, 0);
      return 1;
    }

  return 0;
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
        case 104:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
          fprintf (file, "-");	
	  s390_output_symbolic_const (file, cfun->machine->literal_pool_label);
 	  break;
        case 105:
	  s390_output_symbolic_const (file, cfun->machine->literal_pool_label);
          fprintf (file, "-");
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
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
          fprintf (file, "@PLT-");
	  s390_output_symbolic_const (file, cfun->machine->literal_pool_label);
	  break;
	case UNSPEC_TLSGD:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@TLSGD");
	  break;
	case UNSPEC_TLSLDM:
	  assemble_name (file, get_some_local_dynamic_name ());
	  fprintf (file, "@TLSLDM");
	  break;
	case UNSPEC_DTPOFF:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@DTPOFF");
	  break;
	case UNSPEC_NTPOFF:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@NTPOFF");
	  break;
	case UNSPEC_GOTNTPOFF:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@GOTNTPOFF");
	  break;
	case UNSPEC_INDNTPOFF:
	  s390_output_symbolic_const (file, XVECEXP (x, 0, 0));
	  fprintf (file, "@INDNTPOFF");
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

  if (!s390_decompose_address (addr, &ad)
      || (ad.base && !REG_OK_FOR_BASE_STRICT_P (ad.base))
      || (ad.indx && !REG_OK_FOR_INDEX_STRICT_P (ad.indx)))
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
    'J': print tls_load/tls_gdcall/tls_ldcall suffix
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

    case 'J':
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  fprintf (file, "%s", ":tls_load:");
	  output_addr_const (file, x);
	}
      else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSGD)
	{
	  fprintf (file, "%s", ":tls_gdcall:");
	  output_addr_const (file, XVECEXP (x, 0, 0));
	}
      else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSLDM)
	{
	  fprintf (file, "%s", ":tls_ldcall:");
	  assemble_name (file, get_some_local_dynamic_name ());
	}
      else
	abort ();
      return;

    case 'O':
      {
        struct s390_address ad;

        if (GET_CODE (x) != MEM
            || !s390_decompose_address (XEXP (x, 0), &ad)
	    || (ad.base && !REG_OK_FOR_BASE_STRICT_P (ad.base))
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
            || !s390_decompose_address (XEXP (x, 0), &ad)
	    || (ad.base && !REG_OK_FOR_BASE_STRICT_P (ad.base))
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
      if (GET_CODE (target) == STRICT_LOW_PART)
	target = XEXP (target, 0);
      while (GET_CODE (target) == SUBREG)
	target = SUBREG_REG (target);

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


/* Split all branches that exceed the maximum distance.  
   Returns true if this created a new literal pool entry.  

   Code generated by this routine is allowed to use
   TEMP_REG as temporary scratch register.  If this is
   done, TEMP_USED is set to true.  */

static int 
s390_split_branches (temp_reg, temp_used)
     rtx temp_reg;
     bool *temp_used;
{
  int new_literal = 0;
  rtx insn, pat, tmp, target;
  rtx *label;

  /* We need correct insn addresses.  */

  shorten_branches (get_insns ());

  /* Find all branches that exceed 64KB, and split them.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) != JUMP_INSN)
	continue;

      pat = PATTERN (insn);
      if (GET_CODE (pat) == PARALLEL && XVECLEN (pat, 0) > 2)
	pat = XVECEXP (pat, 0, 0);
      if (GET_CODE (pat) != SET || SET_DEST (pat) != pc_rtx)
	continue;

      if (GET_CODE (SET_SRC (pat)) == LABEL_REF) 
	{
	  label = &SET_SRC (pat);
	} 
      else if (GET_CODE (SET_SRC (pat)) == IF_THEN_ELSE) 
	{
	  if (GET_CODE (XEXP (SET_SRC (pat), 1)) == LABEL_REF) 
	    label = &XEXP (SET_SRC (pat), 1);
          else if (GET_CODE (XEXP (SET_SRC (pat), 2)) == LABEL_REF) 
            label = &XEXP (SET_SRC (pat), 2);
	  else
	    continue;
        }
      else
	continue;

      if (get_attr_length (insn) <= (TARGET_64BIT ? 6 : 4))
	continue;

      *temp_used = 1;

      if (TARGET_64BIT)
	{
	  tmp = emit_insn_before (gen_rtx_SET (Pmode, temp_reg, *label), insn);
	  INSN_ADDRESSES_NEW (tmp, -1);

	  target = temp_reg;
	}
      else if (!flag_pic)
	{
	  new_literal = 1;
	  tmp = force_const_mem (Pmode, *label);
	  tmp = emit_insn_before (gen_rtx_SET (Pmode, temp_reg, tmp), insn);
	  INSN_ADDRESSES_NEW (tmp, -1);

	  target = temp_reg;
	}
      else
	{
	  new_literal = 1;
	  tmp = gen_rtx_UNSPEC (SImode, gen_rtvec (1, *label), 104);
	  tmp = gen_rtx_CONST (SImode, tmp);
	  tmp = force_const_mem (SImode, tmp);
	  tmp = emit_insn_before (gen_rtx_SET (Pmode, temp_reg, tmp), insn);
	  INSN_ADDRESSES_NEW (tmp, -1);

	  target = gen_rtx_REG (Pmode, BASE_REGISTER);
	  target = gen_rtx_PLUS (Pmode, target, temp_reg);
	}

      if (!validate_change (insn, label, target, 0))
	abort ();
    }

  return new_literal;
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

/* Check whether ADDR is an address that uses the base register, 
   without actually constituting a literal pool access.  (This happens
   in 31-bit PIC mode, where the base register is used as anchor for
   relative addressing of local symbols.) 

   Returns 1 if the base register occupies the base slot,
   returns 2 if the base register occupies the index slot,
   returns 0 if the address is not of this form.  */

static int
find_base_register_in_addr (addr)
     struct s390_address *addr;
{
  /* If DISP is complex, we might have a literal pool reference.  */
  if (addr->disp && GET_CODE (addr->disp) != CONST_INT)
    return 0;

  if (addr->base && REG_P (addr->base) && REGNO (addr->base) == BASE_REGISTER)
    return 1;

  if (addr->indx && REG_P (addr->indx) && REGNO (addr->indx) == BASE_REGISTER)
    return 2;

  return 0;
}

/* Return true if X contains an address that uses the base register, 
   without actually constituting a literal pool access.  */

static bool
find_base_register_ref (x)
     rtx x;
{
  bool retv = FALSE;
  struct s390_address addr;
  int i, j;
  const char *fmt;

  /* Addresses can only occur inside a MEM ...  */
  if (GET_CODE (x) == MEM)
    {
      if (s390_decompose_address (XEXP (x, 0), &addr)
	  && find_base_register_in_addr (&addr))
	return TRUE;
    }

  /* ... or a load-address type pattern.  */
  if (GET_CODE (x) == SET && GET_CODE (SET_DEST (x)) == REG)
    {
      if (s390_decompose_address (SET_SRC (x), &addr)
	  && find_base_register_in_addr (&addr))
	return TRUE;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        {
          retv |= find_base_register_ref (XEXP (x, i));
        }
      else if (fmt[i] == 'E')
        {
          for (j = 0; j < XVECLEN (x, i); j++)
            retv |= find_base_register_ref (XVECEXP (x, i, j));
        }
    }

  return retv;
}

/* If X contains an address that uses the base register,
   without actually constituting a literal pool access,
   replace the base register with REPL in all such cases.

   Handles both MEMs and load address patterns.  */

static void
replace_base_register_ref (x, repl)
     rtx *x;
     rtx repl;
{
  struct s390_address addr;
  rtx new_addr;
  int i, j, pos;
  const char *fmt;

  /* Addresses can only occur inside a MEM ...  */
  if (GET_CODE (*x) == MEM)
    {
      if (s390_decompose_address (XEXP (*x, 0), &addr)
	  && (pos = find_base_register_in_addr (&addr)))
	{
	  if (pos == 1)
	    addr.base = repl;
	  else
	    addr.indx = repl;

	  new_addr = addr.base;
	  if (addr.indx)
	    new_addr = gen_rtx_PLUS (Pmode, new_addr, addr.indx);
	  if (addr.disp)
	    new_addr = gen_rtx_PLUS (Pmode, new_addr, addr.disp);

	  *x = replace_equiv_address (*x, new_addr);
	  return;
	}
    }

  /* ... or a load-address type pattern.  */
  if (GET_CODE (*x) == SET && GET_CODE (SET_DEST (*x)) == REG)
    {
      if (s390_decompose_address (SET_SRC (*x), &addr)
	  && (pos = find_base_register_in_addr (&addr)))
	{
	  if (pos == 1)
	    addr.base = repl;
	  else
	    addr.indx = repl;

	  new_addr = addr.base;
	  if (addr.indx)
	    new_addr = gen_rtx_PLUS (Pmode, new_addr, addr.indx);
	  if (addr.disp)
	    new_addr = gen_rtx_PLUS (Pmode, new_addr, addr.disp);

	  SET_SRC (*x) = new_addr;
	  return;
	}
    }

  fmt = GET_RTX_FORMAT (GET_CODE (*x));
  for (i = GET_RTX_LENGTH (GET_CODE (*x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        {
          replace_base_register_ref (&XEXP (*x, i), repl);
        }
      else if (fmt[i] == 'E')
        {
          for (j = 0; j < XVECLEN (*x, i); j++)
            replace_base_register_ref (&XVECEXP (*x, i, j), repl);
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
  rtx pool_insn;
  bitmap insns;

  struct constant *constants[NR_C_MODES];
  rtx label;
  int size;
  bool anchor;
};

static struct constant_pool * s390_chunkify_start PARAMS ((rtx, bool *));
static void s390_chunkify_finish PARAMS ((struct constant_pool *, rtx));
static void s390_chunkify_cancel PARAMS ((struct constant_pool *));

static struct constant_pool *s390_start_pool PARAMS ((struct constant_pool **, rtx));
static void s390_end_pool PARAMS ((struct constant_pool *, rtx));
static void s390_add_pool_insn PARAMS ((struct constant_pool *, rtx));
static struct constant_pool *s390_find_pool PARAMS ((struct constant_pool *, rtx));
static void s390_add_constant PARAMS ((struct constant_pool *, rtx, enum machine_mode));
static rtx s390_find_constant PARAMS ((struct constant_pool *, rtx, enum machine_mode));
static void s390_add_anchor PARAMS ((struct constant_pool *));
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
  pool->pool_insn = NULL_RTX;
  pool->insns = BITMAP_XMALLOC ();
  pool->size = 0;
  pool->anchor = FALSE;

  for (prev = pool_list; *prev; prev = &(*prev)->next)
    ;
  *prev = pool;

  return pool;
}

/* End range of instructions covered by POOL at INSN and emit
   placeholder insn representing the pool.  */

static void
s390_end_pool (pool, insn)
     struct constant_pool *pool;
     rtx insn;
{
  rtx pool_size = GEN_INT (pool->size + 8 /* alignment slop */);

  if (!insn)
    insn = get_last_insn ();

  pool->pool_insn = emit_insn_after (gen_pool (pool_size), insn);
  INSN_ADDRESSES_NEW (pool->pool_insn, -1);
}

/* Add INSN to the list of insns covered by POOL.  */

static void
s390_add_pool_insn (pool, insn)
     struct constant_pool *pool;
     rtx insn;
{
  bitmap_set_bit (pool->insns, INSN_UID (insn));
}

/* Return pool out of POOL_LIST that covers INSN.  */

static struct constant_pool *
s390_find_pool (pool_list, insn)
     struct constant_pool *pool_list;
     rtx insn;
{
  struct constant_pool *pool;

  for (pool = pool_list; pool; pool = pool->next)
    if (bitmap_bit_p (pool->insns, INSN_UID (insn)))
      break;

  return pool;
}

/* Add constant VAL of mode MODE to the constant pool POOL.  */

static void
s390_add_constant (pool, val, mode)
     struct constant_pool *pool;
     rtx val;
     enum machine_mode mode;
{
  struct constant *c;
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
}

/* Find constant VAL of mode MODE in the constant pool POOL.
   Return an RTX describing the distance from the start of
   the pool to the location of the new constant.  */
 
static rtx
s390_find_constant (pool, val, mode)
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
    abort ();
 
  offset = gen_rtx_MINUS (Pmode, gen_rtx_LABEL_REF (Pmode, c->label),
                                 gen_rtx_LABEL_REF (Pmode, pool->label));
  offset = gen_rtx_CONST (Pmode, offset);
  return offset;
}

/* Set 'anchor' flag in POOL.  */

static void
s390_add_anchor (pool)
     struct constant_pool *pool;
{
  if (!pool->anchor)
    {
      pool->anchor = TRUE;
      pool->size += 4;
    }
}

/* Dump out the constants in POOL.  */

static rtx
s390_dump_pool (pool)
     struct constant_pool *pool;
{
  struct constant *c;
  rtx insn;
  int i;

  /* Pool start insn switches to proper section 
     and guarantees necessary alignment.  */
  if (TARGET_64BIT)
    insn = emit_insn_after (gen_pool_start_64 (), pool->pool_insn);
  else
    insn = emit_insn_after (gen_pool_start_31 (), pool->pool_insn);
  INSN_ADDRESSES_NEW (insn, -1);

  insn = emit_label_after (pool->label, insn);
  INSN_ADDRESSES_NEW (insn, -1);

  /* Emit anchor if we need one.  */
  if (pool->anchor)
    {
      rtx anchor = gen_rtx_LABEL_REF (VOIDmode, pool->label);
      anchor = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, anchor), 105);
      anchor = gen_rtx_CONST (VOIDmode, anchor);
      insn = emit_insn_after (gen_consttable_si (anchor), insn);
      INSN_ADDRESSES_NEW (insn, -1);
    }

  /* Dump constants in descending alignment requirement order,
     ensuring proper alignment for every constant.  */
  for (i = 0; i < NR_C_MODES; i++)
    for (c = pool->constants[i]; c; c = c->next)
      {
	/* Convert 104 unspecs to pool-relative references.  */
	rtx value = c->value;
	if (GET_CODE (value) == CONST
	    && GET_CODE (XEXP (value, 0)) == UNSPEC
	    && XINT (XEXP (value, 0), 1) == 104
	    && XVECLEN (XEXP (value, 0), 0) == 1)
	  {
	    value = gen_rtx_MINUS (Pmode, XVECEXP (XEXP (value, 0), 0, 0),
	    			   gen_rtx_LABEL_REF (VOIDmode, pool->label));
	    value = gen_rtx_CONST (VOIDmode, value);
	  }

	insn = emit_label_after (c->label, insn);
	INSN_ADDRESSES_NEW (insn, -1);
	insn = emit_insn_after (gen_consttable[i] (value), insn);
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

  /* Remove placeholder insn.  */
  remove_insn (pool->pool_insn);

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

  BITMAP_XFREE (pool->insns);
  free (pool);
} 


/* Chunkify the literal pool if required.

   Code generated by this routine is allowed to use
   TEMP_REG as temporary scratch register.  If this is
   done, TEMP_USED is set to true.  */

#define S390_POOL_CHUNK_MIN	0xc00
#define S390_POOL_CHUNK_MAX	0xe00

static struct constant_pool * 
s390_chunkify_start (temp_reg, temp_used)
     rtx temp_reg;
     bool *temp_used;
{
  rtx base_reg = gen_rtx_REG (Pmode, BASE_REGISTER);

  struct constant_pool *curr_pool = NULL, *pool_list = NULL;
  int extra_size = 0;
  bitmap far_labels;
  rtx insn;

  rtx (*gen_reload_base) PARAMS ((rtx, rtx)) =
    TARGET_64BIT? gen_reload_base_64 : gen_reload_base_31;


  /* Do we need to chunkify the literal pool?  */

  if (get_pool_size () < S390_POOL_CHUNK_MAX)
    return NULL;

  /* We need correct insn addresses.  */

  shorten_branches (get_insns ());

  /* Scan all insns and move literals to pool chunks.
     Also, emit anchor reload insns before every insn that uses 
     the literal pool base register as anchor pointer.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	{
	  rtx pool_ref = NULL_RTX;
	  find_constant_pool_ref (PATTERN (insn), &pool_ref);
	  if (pool_ref)
	    {
	      if (!curr_pool)
		curr_pool = s390_start_pool (&pool_list, insn);

	      s390_add_constant (curr_pool, get_pool_constant (pool_ref), 
					    get_pool_mode (pool_ref));
	      s390_add_pool_insn (curr_pool, insn);
	    }

	  else if (!TARGET_64BIT && flag_pic
                   && find_base_register_ref (PATTERN (insn)))
	    {
	      rtx new = gen_reload_anchor (temp_reg, base_reg);
	      new = emit_insn_before (new, insn);
	      INSN_ADDRESSES_NEW (new, INSN_ADDRESSES (INSN_UID (insn)));
	      extra_size += 8;
	      *temp_used = 1;
	      
	      if (!curr_pool)
		curr_pool = s390_start_pool (&pool_list, new);

	      s390_add_anchor (curr_pool);
	      s390_add_pool_insn (curr_pool, insn);
	    }
	}

      if (GET_CODE (insn) == JUMP_INSN || GET_CODE (insn) == CODE_LABEL)
	if (curr_pool)
	  s390_add_pool_insn (curr_pool, insn);

      if (!curr_pool 
	  || INSN_ADDRESSES_SIZE () <= (size_t) INSN_UID (insn)
          || INSN_ADDRESSES (INSN_UID (insn)) == -1)
	continue;

      if (TARGET_64BIT)
	{
	  if (curr_pool->size < S390_POOL_CHUNK_MAX)
	    continue;

	  s390_end_pool (curr_pool, NULL_RTX);
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
	  if (GET_CODE (insn) == CODE_LABEL)
	    extra_size += 6;

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
	           || curr_pool->size > S390_POOL_CHUNK_MAX))
	    {
              rtx label, jump, barrier;

	      /* We can insert the barrier only after a 'real' insn.  */
	      if (GET_CODE (insn) != INSN && GET_CODE (insn) != CALL_INSN)
		continue;
	      if (get_attr_length (insn) == 0)
		continue;

	      /* Don't separate insns created by s390_split_branches.  */
	      if (GET_CODE (insn) == INSN 
		  && GET_CODE (PATTERN (insn)) == SET
		  && rtx_equal_p (SET_DEST (PATTERN (insn)), temp_reg))
		continue;

 	      label = gen_label_rtx ();
	      jump = emit_jump_insn_after (gen_jump (label), insn);
	      barrier = emit_barrier_after (jump);
	      insn = emit_label_after (label, barrier);
	      JUMP_LABEL (jump) = label;
	      LABEL_NUSES (label) = 1;

	      INSN_ADDRESSES_NEW (jump, -1);
	      INSN_ADDRESSES_NEW (barrier, -1);
	      INSN_ADDRESSES_NEW (insn, -1);

	      s390_end_pool (curr_pool, barrier);
	      curr_pool = NULL;
	      extra_size = 0;
	    }
	}
    }

  if (curr_pool)
    s390_end_pool (curr_pool, NULL_RTX);


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
	  if (GET_CODE (pat) == PARALLEL && XVECLEN (pat, 0) > 2)
	    pat = XVECEXP (pat, 0, 0);

          if (GET_CODE (pat) == SET) 
            {
	      rtx label = JUMP_LABEL (insn);
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
	    rtx new_insn = gen_reload_base (base_reg, pool->label);
	    INSN_ADDRESSES_NEW (emit_insn_after (new_insn, insn), -1);
	  }
      }


  BITMAP_XFREE (far_labels);


  /* Recompute insn addresses.  */

  init_insn_lengths ();
  shorten_branches (get_insns ());

  return pool_list;
}

/* POOL_LIST is a chunk list as prepared by s390_chunkify_start.
   After we have decided to use this list, finish implementing 
   all changes to the current function as required.

   Code generated by this routine is allowed to use
   TEMP_REG as temporary scratch register.  */
 
static void
s390_chunkify_finish (pool_list, temp_reg)
     struct constant_pool *pool_list;
     rtx temp_reg;
{
  rtx base_reg = gen_rtx_REG (Pmode, BASE_REGISTER);
  struct constant_pool *curr_pool = NULL;
  rtx insn;
 
 
  /* Replace all literal pool references.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn)) 
    {
      curr_pool = s390_find_pool (pool_list, insn);
      if (!curr_pool)
	continue;

      if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
        {
          rtx addr, pool_ref = NULL_RTX;
          find_constant_pool_ref (PATTERN (insn), &pool_ref);
          if (pool_ref)
            {
              addr = s390_find_constant (curr_pool, get_pool_constant (pool_ref),
                                                    get_pool_mode (pool_ref));
              addr = gen_rtx_PLUS (Pmode, base_reg, addr);
              replace_constant_pool_ref (&PATTERN (insn), pool_ref, addr);
              INSN_CODE (insn) = -1;
            }

	  else if (!TARGET_64BIT && flag_pic
                   && find_base_register_ref (PATTERN (insn)))
	    {
	      replace_base_register_ref (&PATTERN (insn), temp_reg);
	    }
        }
    }

  /* Dump out all literal pools.  */
 
  for (curr_pool = pool_list; curr_pool; curr_pool = curr_pool->next)
    s390_dump_pool (curr_pool);
 
  /* Free pool list.  */

  while (pool_list)
    {
      struct constant_pool *next = pool_list->next;
      s390_free_pool (pool_list);
      pool_list = next;
    }
}

/* POOL_LIST is a chunk list as prepared by s390_chunkify_start.
   We have decided we cannot use this list, so revert all changes
   to the current function that were done by s390_chunkify_start.  */
 
static void
s390_chunkify_cancel (pool_list)
     struct constant_pool *pool_list;
{
  struct constant_pool *curr_pool = NULL;
  rtx insn;

  /* Remove all pool placeholder insns.  */

  for (curr_pool = pool_list; curr_pool; curr_pool = curr_pool->next)
    {
      /* Did we insert an extra barrier?  Remove it.  */
      rtx barrier = PREV_INSN (curr_pool->pool_insn);
      rtx jump = barrier? PREV_INSN (barrier) : NULL_RTX;
      rtx label = NEXT_INSN (curr_pool->pool_insn);

      if (jump && GET_CODE (jump) == JUMP_INSN
	  && barrier && GET_CODE (barrier) == BARRIER
	  && label && GET_CODE (label) == CODE_LABEL
	  && GET_CODE (PATTERN (jump)) == SET
	  && SET_DEST (PATTERN (jump)) == pc_rtx
	  && GET_CODE (SET_SRC (PATTERN (jump))) == LABEL_REF
	  && XEXP (SET_SRC (PATTERN (jump)), 0) == label)
	{
	  remove_insn (jump);
	  remove_insn (barrier);
	  remove_insn (label);
	}

      remove_insn (curr_pool->pool_insn);
    }

  /* Remove all base/anchor register reload insns.  */

  for (insn = get_insns (); insn; )
    {
      rtx next_insn = NEXT_INSN (insn);

      if (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SET
	  && GET_CODE (SET_SRC (PATTERN (insn))) == UNSPEC
	  && (XINT (SET_SRC (PATTERN (insn)), 1) == 210
	      || XINT (SET_SRC (PATTERN (insn)), 1) == 211))
	remove_insn (insn);

      insn = next_insn;
    }

  /* Free pool list.  */

  while (pool_list)
    {
      struct constant_pool *next = pool_list->next;
      s390_free_pool (pool_list);
      pool_list = next;
    }
}


/* Index of constant pool chunk that is currently being processed.
   Set to -1 before function output has started.  */
int s390_pool_count = -1;

/* Number of elements of current constant pool.  */
int s390_nr_constants;

/* Output main constant pool to stdio stream FILE.  */ 

void
s390_output_constant_pool (start_label, end_label)
     rtx start_label;
     rtx end_label;
{
  if (TARGET_64BIT) 
    {
      readonly_data_section ();
      ASM_OUTPUT_ALIGN (asm_out_file, 3);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", 
                                 CODE_LABEL_NUMBER (start_label));
    } 
  else 
    {
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
                                 CODE_LABEL_NUMBER (start_label));
      ASM_OUTPUT_ALIGN (asm_out_file, 2);      
    }

  s390_pool_count = 0;
  output_constant_pool (current_function_name, current_function_decl);
  s390_pool_count = -1;
  if (TARGET_64BIT)
    function_section (current_function_decl);
  else
    {
      ASM_OUTPUT_ALIGN (asm_out_file, 1);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (end_label));
    }
}

/* Rework the prolog/epilog to avoid saving/restoring
   registers unnecessarily.  If TEMP_REGNO is nonnegative,
   it specifies the number of a caller-saved register used 
   as temporary scratch register by code emitted during 
   machine dependent reorg.  */

static void
s390_optimize_prolog (temp_regno)
     int temp_regno;
{
  int save_first, save_last, restore_first, restore_last;
  int i, j;
  rtx insn, new_insn, next_insn;

  /* Recompute regs_ever_live data for special registers.  */
  regs_ever_live[BASE_REGISTER] = 0;
  regs_ever_live[RETURN_REGNUM] = 0;
  regs_ever_live[STACK_POINTER_REGNUM] = cfun->machine->frame_size > 0;

  /* If there is (possibly) any pool entry, we need to
     load the base register.  
     ??? FIXME: this should be more precise.  */
  if (get_pool_size ())
    regs_ever_live[BASE_REGISTER] = 1;

  /* In non-leaf functions, the prolog/epilog code relies 
     on RETURN_REGNUM being saved in any case.  */
  if (!current_function_is_leaf)
    regs_ever_live[RETURN_REGNUM] = 1;

  /* We need to save/restore the temporary register.  */
  if (temp_regno >= 0)
    regs_ever_live[temp_regno] = 1;


  /* Find first and last gpr to be saved.  */
  
  for (i = 6; i < 16; i++)
    if (regs_ever_live[i])
      if (!global_regs[i]
	  || i == STACK_POINTER_REGNUM 
          || i == RETURN_REGNUM
          || i == BASE_REGISTER 
          || (flag_pic && i == (int)PIC_OFFSET_TABLE_REGNUM))
	break;

  for (j = 15; j > i; j--)
    if (regs_ever_live[j])
      if (!global_regs[j]
	  || j == STACK_POINTER_REGNUM 
          || j == RETURN_REGNUM
          || j == BASE_REGISTER 
          || (flag_pic && j == (int)PIC_OFFSET_TABLE_REGNUM))
	break;

  if (i == 16)
    {
      /* Nothing to save/restore.  */
      save_first = restore_first = -1;
      save_last = restore_last = -1;
    }
  else
    {
      /* Save/restore from i to j.  */
      save_first = restore_first = i;
      save_last = restore_last = j;
    }

  /* Varargs functions need to save gprs 2 to 6.  */
  if (current_function_stdarg)
    {
      save_first = 2;
      if (save_last < 6)
        save_last = 6;
    }


  /* If all special registers are in fact used, there's nothing we
     can do, so no point in walking the insn list.  */
  if (i <= BASE_REGISTER && j >= BASE_REGISTER
      && i <= RETURN_REGNUM && j >= RETURN_REGNUM)
    return;


  /* Search for prolog/epilog insns and replace them.  */

  for (insn = get_insns (); insn; insn = next_insn)
    {
      int first, last, off;
      rtx set, base, offset;

      next_insn = NEXT_INSN (insn);

      if (GET_CODE (insn) != INSN)
	continue;
      if (GET_CODE (PATTERN (insn)) != PARALLEL)
	continue;

      if (store_multiple_operation (PATTERN (insn), VOIDmode))
	{
	  set = XVECEXP (PATTERN (insn), 0, 0);
	  first = REGNO (SET_SRC (set));
	  last = first + XVECLEN (PATTERN (insn), 0) - 1;
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_DEST (set), 0), &offset);
	  off = INTVAL (offset) - first * UNITS_PER_WORD;

	  if (GET_CODE (base) != REG || off < 0)
	    continue;
	  if (first > BASE_REGISTER && first > RETURN_REGNUM)
	    continue;
	  if (last < BASE_REGISTER && last < RETURN_REGNUM)
	    continue;

	  if (save_first != -1)
	    {
	      new_insn = save_gprs (base, off, save_first, save_last);
	      new_insn = emit_insn_before (new_insn, insn);
	      INSN_ADDRESSES_NEW (new_insn, -1);
	    }

	  remove_insn (insn);
	}

      if (load_multiple_operation (PATTERN (insn), VOIDmode))
	{
	  set = XVECEXP (PATTERN (insn), 0, 0);
	  first = REGNO (SET_DEST (set));
	  last = first + XVECLEN (PATTERN (insn), 0) - 1;
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_SRC (set), 0), &offset);
	  off = INTVAL (offset) - first * UNITS_PER_WORD;

	  if (GET_CODE (base) != REG || off < 0)
	    continue;
	  if (first > BASE_REGISTER && first > RETURN_REGNUM)
	    continue;
	  if (last < BASE_REGISTER && last < RETURN_REGNUM)
	    continue;

	  if (restore_first != -1)
	    {
	      new_insn = restore_gprs (base, off, restore_first, restore_last);
	      new_insn = emit_insn_before (new_insn, insn);
	      INSN_ADDRESSES_NEW (new_insn, -1);
	    }

	  remove_insn (insn);
	}
    }
}

/* Check whether any insn in the function makes use of the original
   value of RETURN_REG (e.g. for __builtin_return_address).
   If so, insert an insn reloading that value.

   Return true if any such insn was found.  */

static bool
s390_fixup_clobbered_return_reg (return_reg)
    rtx return_reg;
{
  bool replacement_done = 0;
  rtx insn;

  /* If we never called __builtin_return_address, register 14
     might have been used as temp during the prolog; we do
     not want to touch those uses.  */
  if (!has_hard_reg_initial_val (Pmode, REGNO (return_reg)))
    return false;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx reg, off, new_insn;

      if (GET_CODE (insn) != INSN)
	continue;
      if (!reg_referenced_p (return_reg, PATTERN (insn)))
	continue;
      if (GET_CODE (PATTERN (insn)) == PARALLEL
	  && store_multiple_operation (PATTERN (insn), VOIDmode))
	continue;

      if (frame_pointer_needed)
	reg = hard_frame_pointer_rtx;
      else
	reg = stack_pointer_rtx;

      off = GEN_INT (cfun->machine->frame_size + REGNO (return_reg) * UNITS_PER_WORD);
      if (INTVAL (off) >= 4096)
	{
	  off = force_const_mem (Pmode, off);
	  new_insn = gen_rtx_SET (Pmode, return_reg, off);
	  new_insn = emit_insn_before (new_insn, insn);
	  INSN_ADDRESSES_NEW (new_insn, -1);
	  off = return_reg;
	}

      new_insn = gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, reg, off));
      new_insn = gen_rtx_SET (Pmode, return_reg, new_insn);
      new_insn = emit_insn_before (new_insn, insn);
      INSN_ADDRESSES_NEW (new_insn, -1);

      replacement_done = 1;
    }

  return replacement_done;
}

/* Perform machine-dependent processing.  */

void
s390_machine_dependent_reorg (first)
     rtx first ATTRIBUTE_UNUSED;
{
  bool fixed_up_clobbered_return_reg = 0;
  rtx temp_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);
  bool temp_used = 0;

  /* Make sure all splits have been performed; splits after
     machine_dependent_reorg might confuse insn length counts.  */
  split_all_insns_noflow ();


  /* There are two problematic situations we need to correct:
 
     - the literal pool might be > 4096 bytes in size, so that
       some of its elements cannot be directly accessed
 
     - a branch target might be > 64K away from the branch, so that
       it is not possible to use a PC-relative instruction.
 
     To fix those, we split the single literal pool into multiple
     pool chunks, reloading the pool base register at various
     points throughout the function to ensure it always points to
     the pool chunk the following code expects, and / or replace
     PC-relative branches by absolute branches.
 
     However, the two problems are interdependent: splitting the
     literal pool can move a branch further away from its target,
     causing the 64K limit to overflow, and on the other hand,
     replacing a PC-relative branch by an absolute branch means
     we need to put the branch target address into the literal
     pool, possibly causing it to overflow.
 
     So, we loop trying to fix up both problems until we manage
     to satisfy both conditions at the same time.  Note that the
     loop is guaranteed to terminate as every pass of the loop
     strictly decreases the total number of PC-relative branches
     in the function.  (This is not completely true as there
     might be branch-over-pool insns introduced by chunkify_start.
     Those never need to be split however.)  */
 
  for (;;)
    {
      struct constant_pool *pool_list;
 
      /* Try to chunkify the literal pool.  */
      pool_list = s390_chunkify_start (temp_reg, &temp_used);

      /* Split out-of-range branches.  If this has created new
	 literal pool entries, cancel current chunk list and
	 recompute it.  */
      if (s390_split_branches (temp_reg, &temp_used))
        {
          if (pool_list)
            s390_chunkify_cancel (pool_list);
 
          continue;
        }

      /* Check whether we have clobbered a use of the return
	 register (e.g. for __builtin_return_address).  If so,
	 add insns reloading the register where necessary.  */
      if (temp_used && !fixed_up_clobbered_return_reg
	  && s390_fixup_clobbered_return_reg (temp_reg))
	{
	  fixed_up_clobbered_return_reg = 1;

	  /* The fixup insns might have caused a jump to overflow.  */
	  if (pool_list)
	    s390_chunkify_cancel (pool_list);

	  continue;
	}
 
      /* If we made it up to here, both conditions are satisfied.
	 Finish up pool chunkification if required.  */
      if (pool_list)
	s390_chunkify_finish (pool_list, temp_reg);
 
      break;
    }
 
  s390_optimize_prolog (temp_used? RETURN_REGNUM : -1);
}


/* Return an RTL expression representing the value of the return address
   for the frame COUNT steps up from the current frame.  FRAME is the
   frame pointer of that frame.  */

rtx
s390_return_addr_rtx (count, frame)
     int count;
     rtx frame;
{
  rtx addr;

  /* For the current frame, we use the initial value of RETURN_REGNUM.
     This works both in leaf and non-leaf functions.  */

  if (count == 0)
    return get_hard_reg_initial_val (Pmode, RETURN_REGNUM);

  /* For frames farther back, we read the stack slot where the
     corresponding RETURN_REGNUM value was saved.  */

  addr = plus_constant (frame, RETURN_REGNUM * UNITS_PER_WORD);
  addr = memory_address (Pmode, addr);
  return gen_rtx_MEM (Pmode, addr);
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
s390_frame_info ()
{
  char gprs_ever_live[16];
  int i, j;
  HOST_WIDE_INT fsize = get_frame_size ();

  if (fsize > 0x7fff0000)
    fatal_error ("Total size of local variables exceeds architecture limit.");

  /* fprs 8 - 15 are caller saved for 64 Bit ABI.  */
  cfun->machine->save_fprs_p = 0;
  if (TARGET_64BIT)
    for (i = 24; i < 32; i++) 
      if (regs_ever_live[i] && !global_regs[i])
	{
          cfun->machine->save_fprs_p = 1;
	  break;
	}

  cfun->machine->frame_size = fsize + cfun->machine->save_fprs_p * 64;

  /* Does function need to setup frame and save area.  */
  
  if (! current_function_is_leaf
      || cfun->machine->frame_size > 0
      || current_function_calls_alloca 
      || current_function_stdarg)
    cfun->machine->frame_size += STARTING_FRAME_OFFSET;

  /* Find first and last gpr to be saved.  Note that at this point,
     we assume the return register and the base register always
     need to be saved.  This is done because the usage of these
     register might change even after the prolog was emitted.
     If it turns out later that we really don't need them, the
     prolog/epilog code is modified again.  */

  for (i = 0; i < 16; i++)
    gprs_ever_live[i] = regs_ever_live[i] && !global_regs[i];

  if (flag_pic)
    gprs_ever_live[PIC_OFFSET_TABLE_REGNUM] =
    regs_ever_live[PIC_OFFSET_TABLE_REGNUM];
  gprs_ever_live[BASE_REGISTER] = 1;
  gprs_ever_live[RETURN_REGNUM] = 1;
  gprs_ever_live[STACK_POINTER_REGNUM] = cfun->machine->frame_size > 0;
  
  for (i = 6; i < 16; i++)
    if (gprs_ever_live[i])
      break;

  for (j = 15; j > i; j--)
    if (gprs_ever_live[j])
      break;


  /* Save / Restore from gpr i to j.  */
  cfun->machine->first_save_gpr = i;
  cfun->machine->first_restore_gpr = i;
  cfun->machine->last_save_gpr  = j;

  /* Varargs functions need to save gprs 2 to 6.  */
  if (current_function_stdarg)
    cfun->machine->first_save_gpr = 2;
}

/* Return offset between argument pointer and frame pointer 
   initially after prologue.  */

int 
s390_arg_frame_offset ()
{
  HOST_WIDE_INT fsize = get_frame_size ();
  int save_fprs_p, i;

  /* fprs 8 - 15 are caller saved for 64 Bit ABI.  */
  save_fprs_p = 0;
  if (TARGET_64BIT)
    for (i = 24; i < 32; i++) 
      if (regs_ever_live[i] && !global_regs[i])
	{
          save_fprs_p = 1;
	  break;
	}

  fsize = fsize + save_fprs_p * 64;

  /* Does function need to setup frame and save area.  */
  
  if (! current_function_is_leaf
      || fsize > 0
      || current_function_calls_alloca 
      || current_function_stdarg)
    fsize += STARTING_FRAME_OFFSET;
  return fsize + STACK_POINTER_OFFSET;
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

/* Generate insn to save registers FIRST to LAST into
   the register save area located at offset OFFSET 
   relative to register BASE.  */

static rtx
save_gprs (base, offset, first, last)
     rtx base;
     int offset;
     int first;
     int last;
{
  rtx addr, insn, note;
  int i;

  addr = plus_constant (base, offset + first * UNITS_PER_WORD);
  addr = gen_rtx_MEM (Pmode, addr);
  set_mem_alias_set (addr, s390_sr_alias_set);

  /* Special-case single register.  */
  if (first == last)
    {
      if (TARGET_64BIT)
        insn = gen_movdi (addr, gen_rtx_REG (Pmode, first));
      else
        insn = gen_movsi (addr, gen_rtx_REG (Pmode, first));

      RTX_FRAME_RELATED_P (insn) = 1;
      return insn;
    }


  insn = gen_store_multiple (addr,
			     gen_rtx_REG (Pmode, first),
			     GEN_INT (last - first + 1));


  /* We need to set the FRAME_RELATED flag on all SETs
     inside the store-multiple pattern.

     However, we must not emit DWARF records for registers 2..5
     if they are stored for use by variable arguments ...  

     ??? Unfortunately, it is not enough to simply not the the
     FRAME_RELATED flags for those SETs, because the first SET
     of the PARALLEL is always treated as if it had the flag
     set, even if it does not.  Therefore we emit a new pattern
     without those registers as REG_FRAME_RELATED_EXPR note.  */

  if (first >= 6)
    {
      rtx pat = PATTERN (insn);

      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (GET_CODE (XVECEXP (pat, 0, i)) == SET)
	  RTX_FRAME_RELATED_P (XVECEXP (pat, 0, i)) = 1;

      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (last >= 6)
    {
      addr = plus_constant (base, offset + 6 * UNITS_PER_WORD);
      note = gen_store_multiple (gen_rtx_MEM (Pmode, addr), 
				 gen_rtx_REG (Pmode, 6),
				 GEN_INT (last - 6 + 1));
      note = PATTERN (note);

      REG_NOTES (insn) =
	gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, 
			   note, REG_NOTES (insn));

      for (i = 0; i < XVECLEN (note, 0); i++)
	if (GET_CODE (XVECEXP (note, 0, i)) == SET)
	  RTX_FRAME_RELATED_P (XVECEXP (note, 0, i)) = 1;

      RTX_FRAME_RELATED_P (insn) = 1;
    }

  return insn;
}

/* Generate insn to restore registers FIRST to LAST from
   the register save area located at offset OFFSET 
   relative to register BASE.  */

static rtx
restore_gprs (base, offset, first, last)
     rtx base;
     int offset;
     int first;
     int last;
{
  rtx addr, insn;

  addr = plus_constant (base, offset + first * UNITS_PER_WORD);
  addr = gen_rtx_MEM (Pmode, addr);
  set_mem_alias_set (addr, s390_sr_alias_set);

  /* Special-case single register.  */
  if (first == last)
    {
      if (TARGET_64BIT)
        insn = gen_movdi (gen_rtx_REG (Pmode, first), addr);
      else
        insn = gen_movsi (gen_rtx_REG (Pmode, first), addr);

      return insn;
    }

  insn = gen_load_multiple (gen_rtx_REG (Pmode, first),
			    addr,
			    GEN_INT (last - first + 1));
  return insn;
}

/* Expand the prologue into a bunch of separate insns.  */

void
s390_emit_prologue ()
{
  rtx insn, addr;
  rtx temp_reg;
  rtx pool_start_label, pool_end_label;
  int i;

  /* Compute frame_info.  */

  s390_frame_info ();

  /* Choose best register to use for temp use within prologue.  */
  
  if (!current_function_is_leaf
      && !has_hard_reg_initial_val (Pmode, RETURN_REGNUM)
      && get_pool_size () < S390_POOL_CHUNK_MAX / 2)
    temp_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);
  else
    temp_reg = gen_rtx_REG (Pmode, 1);

  /* Save call saved gprs.  */

  insn = save_gprs (stack_pointer_rtx, 0, 
		    cfun->machine->first_save_gpr, cfun->machine->last_save_gpr);
  emit_insn (insn);

  /* Dump constant pool and set constant pool register.  */

  pool_start_label = gen_label_rtx();
  pool_end_label = gen_label_rtx();
  cfun->machine->literal_pool_label = pool_start_label;
  
  if (TARGET_64BIT)
    insn = emit_insn (gen_literal_pool_64 (gen_rtx_REG (Pmode, BASE_REGISTER),
                 			   pool_start_label, pool_end_label));
  else
    insn = emit_insn (gen_literal_pool_31 (gen_rtx_REG (Pmode, BASE_REGISTER),
					     pool_start_label, pool_end_label));  
  
  /* Save fprs for variable args.  */

  if (current_function_stdarg)
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
      if (regs_ever_live[18] && !global_regs[18])
	{
	  insn = save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 16, 18);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      if (regs_ever_live[19] && !global_regs[19])
	{
	  insn = save_fpr (stack_pointer_rtx, STACK_POINTER_OFFSET - 8, 19); 
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Decrement stack pointer.  */

  if (cfun->machine->frame_size > 0)
    {
      rtx frame_off = GEN_INT (-cfun->machine->frame_size);

      /* Save incoming stack pointer into temp reg.  */
      
      if (TARGET_BACKCHAIN || cfun->machine->save_fprs_p)
	{
	  insn = emit_insn (gen_move_insn (temp_reg, stack_pointer_rtx));
	}
      
      /* Substract frame size from stack pointer.  */

      frame_off = GEN_INT (-cfun->machine->frame_size);
      if (!CONST_OK_FOR_LETTER_P (-cfun->machine->frame_size, 'K'))
	frame_off = force_const_mem (Pmode, frame_off);

      insn = emit_insn (gen_add2_insn (stack_pointer_rtx, frame_off));
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = 
	gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			   gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				   gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			           GEN_INT (-cfun->machine->frame_size))),
			   REG_NOTES (insn));

      /* Set backchain.  */
      
      if (TARGET_BACKCHAIN)
	{
	  addr = gen_rtx_MEM (Pmode, stack_pointer_rtx);
	  set_mem_alias_set (addr, s390_sr_alias_set);
	  insn = emit_insn (gen_move_insn (addr, temp_reg));
	}

      /* If we support asynchronous exceptions (e.g. for Java),
	 we need to make sure the backchain pointer is set up
	 before any possibly trapping memory access.  */

      if (TARGET_BACKCHAIN && flag_non_call_exceptions)
	{
	  addr = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode));
	  emit_insn (gen_rtx_CLOBBER (VOIDmode, addr));
	}
    }

  /* Save fprs 8 - 15 (64 bit ABI).  */
  
  if (cfun->machine->save_fprs_p)
    {
      insn = emit_insn (gen_add2_insn (temp_reg, GEN_INT(-64)));

      for (i = 24; i < 32; i++)
	if (regs_ever_live[i] && !global_regs[i])
	  {
	    rtx addr = plus_constant (stack_pointer_rtx, 
				      cfun->machine->frame_size - 64 + (i-24)*8);

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
  
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Set up got pointer, if needed.  */
  
  if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
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

          got_symbol = gen_rtx_REG (Pmode, BASE_REGISTER);
          got_symbol = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, got_symbol), 101);
          got_symbol = gen_rtx_PLUS (Pmode, got_symbol, pic_offset_table_rtx);
	  insn = emit_move_insn (pic_offset_table_rtx, got_symbol);
          REG_NOTES(insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, NULL_RTX,
                                               REG_NOTES (insn));
	}
    }      
}

/* Expand the epilogue into a bunch of separate insns.  */

void
s390_emit_epilogue ()
{
  rtx frame_pointer, return_reg;
  int area_bottom, area_top, offset = 0;
  rtvec p;

  /* Check whether to use frame or stack pointer for restore.  */

  frame_pointer = frame_pointer_needed ? 
    hard_frame_pointer_rtx : stack_pointer_rtx;

  /* Compute which parts of the save area we need to access.  */

  if (cfun->machine->first_restore_gpr != -1)
    {
      area_bottom = cfun->machine->first_restore_gpr * UNITS_PER_WORD;
      area_top = (cfun->machine->last_save_gpr + 1) * UNITS_PER_WORD;
    }
  else
    {
      area_bottom = INT_MAX;
      area_top = INT_MIN;
    }

  if (TARGET_64BIT)
    {
      if (cfun->machine->save_fprs_p)
	{
	  if (area_bottom > -64)
	    area_bottom = -64;
	  if (area_top < 0)
	    area_top = 0;
	}
    }
  else
    {
      if (regs_ever_live[18] && !global_regs[18])
	{
	  if (area_bottom > STACK_POINTER_OFFSET - 16)
	    area_bottom = STACK_POINTER_OFFSET - 16;
	  if (area_top < STACK_POINTER_OFFSET - 8)
	    area_top = STACK_POINTER_OFFSET - 8;
	}
      if (regs_ever_live[19] && !global_regs[19])
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
  else if (cfun->machine->frame_size + area_bottom >= 0
           && cfun->machine->frame_size + area_top <= 4096)
    {
      /* Area is in range.  */
      offset = cfun->machine->frame_size;
    }
  else
    {
      rtx insn, frame_off;

      offset = area_bottom < 0 ? -area_bottom : 0; 
      frame_off = GEN_INT (cfun->machine->frame_size - offset);

      if (!CONST_OK_FOR_LETTER_P (INTVAL (frame_off), 'K'))
	frame_off = force_const_mem (Pmode, frame_off);

      insn = emit_insn (gen_add2_insn (frame_pointer, frame_off));
    }

  /* Restore call saved fprs.  */

  if (TARGET_64BIT)
    {
      int i;

      if (cfun->machine->save_fprs_p)
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

  if (cfun->machine->first_restore_gpr != -1)
    {
      rtx insn, addr;
      int i;

      /* Check for global register and save them 
	 to stack location from where they get restored.  */

      for (i = cfun->machine->first_restore_gpr; 
	   i <= cfun->machine->last_save_gpr;
	   i++)
	{
	  /* These registers are special and need to be 
	     restored in any case.  */
	  if (i == STACK_POINTER_REGNUM 
              || i == RETURN_REGNUM
              || i == BASE_REGISTER 
              || (flag_pic && i == (int)PIC_OFFSET_TABLE_REGNUM))
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

      if (!current_function_is_leaf)
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

      insn = restore_gprs (frame_pointer, offset, 
			   cfun->machine->first_restore_gpr, 
			   cfun->machine->last_save_gpr);
      emit_insn (insn);
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
  if (size > 8)
    return 1;

  if (type)
    {
      if (AGGREGATE_TYPE_P (type) &&
          size != 1 && size != 2 && size != 4 && size != 8)
        return 1;

      if (TREE_CODE (type) == COMPLEX_TYPE
          || TREE_CODE (type) == VECTOR_TYPE)
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

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);

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
   STDARG_P is always true, and ignored.
   NEXTARG points to the first anonymous stack argument.

   The following global variables are used to initialize
   the va_list structure:

     current_function_args_info:
       holds number of gprs and fprs used for named arguments.
     current_function_arg_offset_rtx:
       holds the offset of the first anonymous stack argument
       (relative to the virtual arg pointer).  */

void
s390_va_start (valist, nextarg)
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


/* Builtins.  */

enum s390_builtin
{
  S390_BUILTIN_THREAD_POINTER,
  S390_BUILTIN_SET_THREAD_POINTER,

  S390_BUILTIN_max
};

static unsigned int const code_for_builtin_64[S390_BUILTIN_max] = {
  CODE_FOR_get_tp_64,
  CODE_FOR_set_tp_64
};

static unsigned int const code_for_builtin_31[S390_BUILTIN_max] = {
  CODE_FOR_get_tp_31,
  CODE_FOR_set_tp_31
};

static void
s390_init_builtins ()
{
  tree ftype;

  ftype = build_function_type (ptr_type_node, void_list_node);
  builtin_function ("__builtin_thread_pointer", ftype,
		    S390_BUILTIN_THREAD_POINTER, BUILT_IN_MD,
		    NULL, NULL_TREE);

  ftype = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  builtin_function ("__builtin_set_thread_pointer", ftype,
		    S390_BUILTIN_SET_THREAD_POINTER, BUILT_IN_MD,
		    NULL, NULL_TREE);
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
s390_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
#define MAX_ARGS 2

  unsigned int const *code_for_builtin = 
    TARGET_64BIT ? code_for_builtin_64 : code_for_builtin_31;

  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arglist = TREE_OPERAND (exp, 1);
  enum insn_code icode;
  rtx op[MAX_ARGS], pat;
  int arity;
  bool nonvoid;

  if (fcode >= S390_BUILTIN_max)
    internal_error ("bad builtin fcode");
  icode = code_for_builtin[fcode];
  if (icode == 0)
    internal_error ("bad builtin fcode");

  nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;

  for (arglist = TREE_OPERAND (exp, 1), arity = 0;
       arglist;
       arglist = TREE_CHAIN (arglist), arity++)
    {
      const struct insn_operand_data *insn_op;

      tree arg = TREE_VALUE (arglist);
      if (arg == error_mark_node)
	return NULL_RTX;
      if (arity > MAX_ARGS)
	return NULL_RTX;

      insn_op = &insn_data[icode].operand[arity + nonvoid];

      op[arity] = expand_expr (arg, NULL_RTX, insn_op->mode, 0);

      if (!(*insn_op->predicate) (op[arity], insn_op->mode))
	op[arity] = copy_to_mode_reg (insn_op->mode, op[arity]);
    }

  if (nonvoid)
    {
      enum machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
    }

  switch (arity)
    {
    case 0:
      pat = GEN_FCN (icode) (target);
      break;
    case 1:
      if (nonvoid)
        pat = GEN_FCN (icode) (target, op[0]);
      else
	pat = GEN_FCN (icode) (op[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (target, op[0], op[1]);
      break;
    default:
      abort ();
    }
  if (!pat)
    return NULL_RTX;
  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
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
  ASM_GENERATE_INTERNAL_LABEL (label, "LP", labelno);

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

/* Select section for constant in constant pool.  In 32-bit mode,
   constants go in the function section; in 64-bit mode in .rodata.  */

static void
s390_select_rtx_section (mode, x, align)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x ATTRIBUTE_UNUSED;
     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
{
  if (TARGET_64BIT)
    readonly_data_section ();
  else
    function_section (current_function_decl);
}

/* Encode symbol attributes (local vs. global, tls model) of a SYMBOL_REF
   into its name and SYMBOL_REF_FLAG.  */

static void
s390_encode_section_info (decl, first)
     tree decl;
     int first ATTRIBUTE_UNUSED;
{
  bool local_p = (*targetm.binds_local_p) (decl);
  rtx rtl, symbol;

  rtl = DECL_P (decl) ? DECL_RTL (decl) : TREE_CST_RTL (decl);
  if (GET_CODE (rtl) != MEM)
    return;
  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  /* When using PIC, SYMBOL_REF_FLAG marks non-global symbols
     that can be accessed directly.  */
  if (flag_pic)
    SYMBOL_REF_FLAG (symbol) = local_p;

  /* Encode thread-local data with %[GLil] for "global dynamic",
     "local dynamic", "initial exec" or "local exec" TLS models,
     respectively.  */

  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL (decl))
    {
      const char *symbol_str = XSTR (symbol, 0);
      char *newstr;
      size_t len;
      enum tls_model kind = decl_tls_model (decl);

      if (!flag_pic)
	{
	  /* We don't allow non-pic code for shared libraries,
	     so don't generate GD/LD TLS models for non-pic code.  */
	  switch (kind)
	    {
	    case TLS_MODEL_GLOBAL_DYNAMIC:
	      kind = TLS_MODEL_INITIAL_EXEC; break;
	    case TLS_MODEL_LOCAL_DYNAMIC:
	      kind = TLS_MODEL_LOCAL_EXEC; break;
	    default:
	      break;
	    }
	}

      if (symbol_str[0] == '%')
	{
	  if (symbol_str[1] == tls_model_chars[kind])
	    return;
	  symbol_str += 2;
	}
      len = strlen (symbol_str) + 1;
      newstr = alloca (len + 2);

      newstr[0] = '%';
      newstr[1] = tls_model_chars[kind];
      memcpy (newstr + 2, symbol_str, len);

      XSTR (symbol, 0) = ggc_alloc_string (newstr, len + 2 - 1);
    }

  /* If a variable has a forced alignment to < 2 bytes, mark it
     with '@' to prevent it from being used as LARL operand.  */

  else if (TREE_CODE (decl) == VAR_DECL 
	   && DECL_USER_ALIGN (decl) && DECL_ALIGN (decl) < 16
	   && XSTR (symbol, 0)[0] != '@')
    {
      const char *symbol_str = XSTR (symbol, 0);
      size_t len = strlen (symbol_str) + 1;
      char *newstr = alloca (len + 1);

      newstr[0] = '@';
      memcpy (newstr + 1, symbol_str, len);

      XSTR (symbol, 0) = ggc_alloc_string (newstr, len + 1 - 1);
    }
}

/* Undo the above when printing symbol names.  */

static const char *
s390_strip_name_encoding (str)
     const char *str;
{
  if (str[0] == '%')
    str += 2;
  if (str[0] == '@')
    str += 1;
  if (str[0] == '*')
    str += 1;
  return str;
}

/* Output thunk to FILE that implements a C++ virtual function call (with
   multiple inheritance) to FUNCTION.  The thunk adjusts the this pointer 
   by DELTA, and unless VCALL_OFFSET is zero, applies an additional adjustment
   stored at VCALL_OFFSET in the vtable whose address is located at offset 0
   relative to the resulting this pointer.  */

static void
s390_output_mi_thunk (file, thunk, delta, vcall_offset, function)
     FILE *file;
     tree thunk ATTRIBUTE_UNUSED;
     HOST_WIDE_INT delta;
     HOST_WIDE_INT vcall_offset;
     tree function;
{
  rtx op[10];
  int nonlocal = 0;

  /* Operand 0 is the target function.  */
  op[0] = XEXP (DECL_RTL (function), 0);
  if (flag_pic && !SYMBOL_REF_FLAG (op[0]))
    {
      nonlocal = 1;
      op[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op[0]),
			      TARGET_64BIT ? 113 : flag_pic == 2 ? 112 : 110);
      op[0] = gen_rtx_CONST (Pmode, op[0]);
    }

  /* Operand 1 is the 'this' pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function))))
    op[1] = gen_rtx_REG (Pmode, 3);
  else
    op[1] = gen_rtx_REG (Pmode, 2);

  /* Operand 2 is the delta.  */
  op[2] = GEN_INT (delta);

  /* Operand 3 is the vcall_offset.  */
  op[3] = GEN_INT (vcall_offset);

  /* Operand 4 is the temporary register.  */
  op[4] = gen_rtx_REG (Pmode, 1);

  /* Operands 5 to 8 can be used as labels.  */
  op[5] = NULL_RTX;
  op[6] = NULL_RTX;
  op[7] = NULL_RTX;
  op[8] = NULL_RTX;

  /* Operand 9 can be used for temporary register.  */
  op[9] = NULL_RTX;

  /* Generate code.  */
  if (TARGET_64BIT)
    {
      /* Setup literal pool pointer if required.  */
      if (!CONST_OK_FOR_LETTER_P (delta, 'K')
	  || !CONST_OK_FOR_LETTER_P (vcall_offset, 'K'))
	{
	  op[5] = gen_label_rtx ();
	  output_asm_insn ("larl\t%4,%5", op);
	}

      /* Add DELTA to this pointer.  */
      if (delta)
	{
	  if (CONST_OK_FOR_LETTER_P (delta, 'J'))
	    output_asm_insn ("la\t%1,%2(%1)", op);
	  else if (CONST_OK_FOR_LETTER_P (delta, 'K'))
	    output_asm_insn ("aghi\t%1,%2", op);
	  else
	    {
	      op[6] = gen_label_rtx ();
	      output_asm_insn ("agf\t%1,%6-%5(%4)", op);
	    }
	}

      /* Perform vcall adjustment.  */
      if (vcall_offset)
	{
	  if (CONST_OK_FOR_LETTER_P (vcall_offset, 'J'))
	    {
	      output_asm_insn ("lg\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,%3(%4)", op);
	    }
	  else if (CONST_OK_FOR_LETTER_P (vcall_offset, 'K'))
	    {
	      output_asm_insn ("lghi\t%4,%3", op);
	      output_asm_insn ("ag\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,0(%4)", op);
	    }
	  else
	    {
	      op[7] = gen_label_rtx ();
	      output_asm_insn ("llgf\t%4,%7-%5(%4)", op);
	      output_asm_insn ("ag\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,0(%4)", op);
	    }
	}
        
      /* Jump to target.  */
      output_asm_insn ("jg\t%0", op);

      /* Output literal pool if required.  */
      if (op[5])
	{
	  output_asm_insn (".align\t4", op);
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[5]));
	}
      if (op[6])
	{
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[6]));
	  output_asm_insn (".long\t%2", op);
	}
      if (op[7])
	{
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[7]));
	  output_asm_insn (".long\t%3", op);
	}
    }
  else
    {
      /* Setup base pointer if required.  */
      if (!vcall_offset
	  || !CONST_OK_FOR_LETTER_P (delta, 'K')
	  || !CONST_OK_FOR_LETTER_P (vcall_offset, 'K'))
	{
	  op[5] = gen_label_rtx ();
	  output_asm_insn ("basr\t%4,0", op);
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[5]));
	}

      /* Add DELTA to this pointer.  */
      if (delta)
	{
	  if (CONST_OK_FOR_LETTER_P (delta, 'J'))
	    output_asm_insn ("la\t%1,%2(%1)", op);
	  else if (CONST_OK_FOR_LETTER_P (delta, 'K'))
	    output_asm_insn ("ahi\t%1,%2", op);
	  else
	    {
	      op[6] = gen_label_rtx ();
	      output_asm_insn ("a\t%1,%6-%5(%4)", op);
	    }
	}

      /* Perform vcall adjustment.  */
      if (vcall_offset)
        {
	  if (CONST_OK_FOR_LETTER_P (vcall_offset, 'J'))
	    {
	      output_asm_insn ("lg\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,%3(%4)", op);
	    }
	  else if (CONST_OK_FOR_LETTER_P (vcall_offset, 'K'))
	    {
	      output_asm_insn ("lhi\t%4,%3", op);
	      output_asm_insn ("a\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,0(%4)", op);
	    }
	  else
	    {
	      op[7] = gen_label_rtx ();
	      output_asm_insn ("l\t%4,%7-%5(%4)", op);
	      output_asm_insn ("a\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,0(%4)", op);
	    }

	  /* We had to clobber the base pointer register.
	     Re-setup the base pointer (with a different base).  */
	  op[5] = gen_label_rtx ();
	  output_asm_insn ("basr\t%4,0", op);
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[5]));
	}

      /* Jump to target.  */
      op[8] = gen_label_rtx ();

      if (!flag_pic)
	output_asm_insn ("l\t%4,%8-%5(%4)", op);
      else if (!nonlocal)
	output_asm_insn ("a\t%4,%8-%5(%4)", op);
      /* We cannot call through .plt, since .plt requires %r12 loaded.  */
      else if (flag_pic == 1)
	{
	  output_asm_insn ("a\t%4,%8-%5(%4)", op);
	  output_asm_insn ("l\t%4,%0(%4)", op);
	}
      else if (flag_pic == 2)
	{
	  op[9] = gen_rtx_REG (Pmode, 0);
	  output_asm_insn ("l\t%9,%8-4-%5(%4)", op);
	  output_asm_insn ("a\t%4,%8-%5(%4)", op);
	  output_asm_insn ("ar\t%4,%9", op);
	  output_asm_insn ("l\t%4,0(%4)", op);
	}

      output_asm_insn ("br\t%4", op);

      /* Output literal pool.  */
      output_asm_insn (".align\t4", op);

      if (nonlocal && flag_pic == 2)
	output_asm_insn (".long\t%0", op);
      if (nonlocal)
	{
	  op[0] = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
	  SYMBOL_REF_FLAG (op[0]) = 1;
	}

      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[8]));
      if (!flag_pic)
	output_asm_insn (".long\t%0", op);
      else
	output_asm_insn (".long\t%0-%5", op);

      if (op[6])
	{
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[6]));
	  output_asm_insn (".long\t%2", op);
	}
      if (op[7])
	{
	  ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (op[7]));
	  output_asm_insn (".long\t%3", op);
	}
    }
}

/* How to allocate a 'struct machine_function'.  */

static struct machine_function *
s390_init_machine_status ()
{
  return ggc_alloc_cleared (sizeof (struct machine_function));
}

#include "gt-s390.h"
