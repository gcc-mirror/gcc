/* Subroutines for insn-output.c for MIL-STD-1750.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999,
   2000 Free Software Foundation, Inc.
   Contributed by O.M.Kellogg, DASA (kellogg@space.otn.dasa.de)

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

#define __datalbl
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#define HAVE_cc0
#include "conditions.h"
#include "real.h"
#include "regs.h"
#include "output.h"
#include "tm_p.h"

struct datalabel_array datalbl[DATALBL_ARRSIZ];
int datalbl_ndx = -1;
struct jumplabel_array jmplbl[JMPLBL_ARRSIZ];
int jmplbl_ndx = -1;
int label_pending = 0, program_counter = 0;
enum section current_section = Normal;
const char *const sectname[4] =
{"Init", "Normal", "Konst", "Static"};

static int which_bit PARAMS ((int));

void
notice_update_cc (exp)
     rtx exp;
{
  if (GET_CODE (exp) == SET)
    {
      enum rtx_code src_code = GET_CODE (SET_SRC (exp));
      /* Jumps do not alter the cc's.  */
      if (SET_DEST (exp) == pc_rtx)
	return;
      /* Moving a register or constant into memory doesn't alter the cc's. */
      if (GET_CODE (SET_DEST (exp)) == MEM
	  && (src_code == REG || src_code == CONST_INT))
	return;
      /* Function calls clobber the cc's.  */
      if (src_code == CALL)
	{
	  CC_STATUS_INIT;
	  return;
	}
      /* Emulated longword bit-ops leave cc's incorrect */
      if (GET_MODE (SET_DEST (exp)) == HImode ?
	       src_code == AND || src_code == IOR ||
	       src_code == XOR || src_code == NOT : 0)
	{
	  CC_STATUS_INIT;
	  return;
	}
      /* Tests and compares set the cc's in predictable ways.  */
      if (SET_DEST (exp) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  cc_status.value1 = SET_SRC (exp);
	  return;
	}
      /* Anything else will set cc_status. */
      cc_status.flags = CC_NO_OVERFLOW;
      cc_status.value1 = SET_SRC (exp);
      cc_status.value2 = SET_DEST (exp);
      return;
    }
  else if (GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
      if (SET_DEST (XVECEXP (exp, 0, 0)) == pc_rtx)
	return;
      if (SET_DEST (XVECEXP (exp, 0, 0)) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  cc_status.value1 = SET_SRC (XVECEXP (exp, 0, 0));
	  return;
	}
      CC_STATUS_INIT;
    }
  else
    {
      CC_STATUS_INIT;
    }
}


rtx
function_arg (cum, mode, type, named)
     int cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int size;

  if (MUST_PASS_IN_STACK (mode, type))
    return (rtx) 0;
  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);
  if (cum + size < 12)
    return gen_rtx_REG (mode, cum);
  else
    return (rtx) 0;
}


double
get_double (x)
     rtx x;
{
  union
    {
      double d;
      long i[2];
    }
  du;

  du.i[0] = CONST_DOUBLE_LOW (x);
  du.i[1] = CONST_DOUBLE_HIGH (x);
  return du.d;
}

char *
float_label (code, value)
     int code;
     double value;
{
  static char label[32];
  char *p;

  label[0] = code;
  p = label + 1;
  sprintf (p, "%f", value);
  while (*p)
    {
      *p = (*p == '+') ? 'p' :
	(*p == '-') ? 'm' : *p;
      p++;
    }
  return xstrdup (label);
}


const char *
movcnt_regno_adjust (op)
     rtx *op;
{
  static char outstr[80];
  int op0r = REGNO (op[0]), op1r = REGNO (op[1]), op2r = REGNO (op[2]);
#define dstreg op0r
#define srcreg op1r
#define cntreg op2r
#define cntreg_1750 (op0r + 1)

  if (cntreg == cntreg_1750)
    sprintf (outstr, "mov r%d,r%d", op0r, op1r);
  else if (dstreg + 1 == srcreg && cntreg > srcreg)
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%d,r%d", op2r, op1r, op0r, op2r);
  else if (dstreg == cntreg + 1)
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%d,r%d", op0r, op2r, op2r, op1r);
  else if (dstreg == srcreg + 1)
    sprintf (outstr, "xwr r%d,r%d\n\txwr r%d,r%d\n\tmov r%d,r%d",
	     op0r, op1r, op0r, op2r, op1r, op2r);
  else if (cntreg + 1 == srcreg)
    sprintf (outstr, "xwr r%d,r%d\n\txwr r%d,r%d\n\tmov r%d,r%d",
	     op2r, op1r, op0r, op2r, op2r, op0r);
  else if (cntreg == srcreg + 1)
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%d,r%d", op0r, op1r, op1r, op0r);
  else
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%d,r%d\n\txwr r%d,r%d",
	     op2r, cntreg_1750, op0r, op1r, op2r, cntreg_1750);
  return outstr;
}

const char *
mod_regno_adjust (instr, op)
     const char *instr;
     rtx *op;
{
  static char outstr[40];
  const char *r = (!strncmp (instr, "dvr", 3) ? "r" : "");
  int modregno_gcc = REGNO (op[3]), modregno_1750 = REGNO (op[0]) + 1;

  if (modregno_gcc == modregno_1750
      || (reg_renumber != NULL
	  && reg_renumber[modregno_gcc] >= 0
	  && reg_renumber[modregno_gcc] == reg_renumber[modregno_1750]))
    sprintf (outstr, "%s r%%0,%s%%2", instr, r);
  else
    sprintf (outstr, "lr r%d,r%d\n\t%s r%%0,%s%%2\n\txwr r%d,r%d",
	     modregno_gcc, modregno_1750, instr, r, modregno_1750,
	     modregno_gcc);
  return outstr;
}


/* Check if op is a valid memory operand for 1750A Load/Store instructions
   (memory indirection permitted.)  */

int
memop_valid (op)
     rtx op;
{
  static int recurred = 0;
  int valid_operand;

  if (GET_MODE (op) != Pmode && GET_MODE (op) != VOIDmode
      && GET_MODE (op) != QImode)
    return 0;
  switch (GET_CODE (op))
    {
    case MEM:
      if (!recurred && GET_CODE (XEXP (op, 0)) == REG)
	return 1;
    case MINUS:
    case MULT:
    case DIV:
      return 0;
    case PLUS:
      recurred = 1;
      valid_operand = memop_valid (XEXP (op, 0));
      if (valid_operand)
	valid_operand = memop_valid (XEXP (op, 1));
       recurred = 0;
       return valid_operand;
    case REG:
      if (REGNO (op) > 0)
	return 1;
      return 0;
    case CONST:
    case CONST_INT:
    case SYMBOL_REF:
    case SUBREG:
      return 1;
    default:
      printf ("memop_valid: code=%d\n", (int) GET_CODE (op));
      return 1;
    }
}


/* predicate for the MOV instruction: */
int
mov_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == REG);
}

/* predicate for the STC instruction: */
int
small_nonneg_const (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT && INTVAL (op) >= 0 && INTVAL (op) <= 15)
    return 1;
  return 0;
}

/* predicate for constant zero: */
int
zero_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return op == CONST0_RTX (mode);
}


/* predicate for 1750 `B' addressing mode (Base Register with Offset)
   memory operand */
int
b_mode_operand (op)
     rtx op;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inner = XEXP (op, 0);
      if (GET_CODE (inner) == REG && REG_OK_FOR_INDEX_P (inner))
	return 1;
      if (GET_CODE (inner) == PLUS)
	{
	  rtx plus_op0 = XEXP (inner, 0);
	  if (GET_CODE (plus_op0) == REG && REG_OK_FOR_INDEX_P (plus_op0))
	    {
	      rtx plus_op1 = XEXP (inner, 1);
	      if (GET_CODE (plus_op1) == CONST_INT
		  && INTVAL (plus_op1) >= 0
		  && INTVAL (plus_op1) <= 255)
		return 1;
	    }
	}
    }
  return 0;
}


/* Decide whether to output a conditional jump as a "Jump Conditional"
   or as a "Branch Conditional": */

int
find_jmplbl (labelnum)
     int labelnum;
{
  int i, found = 0;

  for (i = 0; i <= jmplbl_ndx; i++)
    if (labelnum == jmplbl[i].num)
      {
	found = 1;
	break;
      }
  if (found)
    return i;
  return -1;
}

const char *
branch_or_jump (condition, targetlabel_number)
     const char *condition;
     int targetlabel_number;
{
  static char buf[30];
  int index;

  if ((index = find_jmplbl (targetlabel_number)) >= 0)
    if (program_counter - jmplbl[index].pc < 128)
      {
	sprintf (buf, "b%s %%l0", condition);
	return buf;
      }
  sprintf (buf, "jc %s,%%l0", condition);
  return buf;
}


int
unsigned_comparison_operator (insn)
     rtx insn;
{
  switch (GET_CODE (insn))
    {
    case GEU:
    case GTU:
    case LEU:
    case LTU:
      return 1;
    default:
      return 0;
    }
}

int
next_cc_user_is_unsigned (insn)
     rtx insn;
{
  if ( !(insn = next_cc0_user (insn)))
    abort ();
  else if (GET_CODE (insn) == JUMP_INSN
	   && GET_CODE (PATTERN (insn)) == SET
	   && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE)
    return unsigned_comparison_operator (XEXP (SET_SRC (PATTERN (insn)), 0));
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (PATTERN (insn)) == SET)
    return unsigned_comparison_operator (SET_SRC (PATTERN (insn)));
  else
    abort ();
}


static int addr_inc;

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the `%' specification that was used to request
   printing of the operand.  If the specification was just `%DIGIT'
   then CODE is 0; if the specification was `%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is
   `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   The 1750 specific codes are:
   'J' for the negative of a constant
   'Q' for printing addresses in B mode syntax
   'd' for the second register in a pair
   't' for the third register in a triple 
   'b' for the bit number (using 1750 test bit convention)
   'B' for the bit number of the 1's complement (for bit clear)
   'w' for int - 16
*/

void
print_operand (file, x, letter)
     FILE *file;
     rtx x;
     int letter;
{
  switch (GET_CODE (x))
    {
    case REG:
      if (letter == 'd')
	fprintf (file, "%d", REGNO (x) + 1);
      else if (letter == 't')
	fprintf (file, "%d", REGNO (x) + 2);
      else
	fprintf (file, "%d", REGNO (x));
      break;

    case SYMBOL_REF:
      fprintf (file, "%s", XSTR (x, 0));
      if (letter == 'A')
	fprintf (file, "+1");
      break;

    case LABEL_REF:
    case CONST:
    case MEM:
      if (letter == 'Q')
	{
	  rtx inner = XEXP (x, 0);
	  switch (GET_CODE (inner))
	    {
	    case REG:
	      fprintf (file, "r%d,0", REGNO (inner));
	      break;
	    case PLUS:
	      fprintf (file, "r%d,%d", REGNO (XEXP (inner, 0)),
		       INTVAL (XEXP (inner, 1)));
	      break;
	    default:
	      fprintf (file, "[ill Q code=%d]", GET_CODE (inner));
	    }
	}
      else
	{
	  addr_inc = (letter == 'A' ? 1 : 0);
	  output_address (XEXP (x, 0));
	}
      break;

    case CONST_DOUBLE:
/*    {
	double value = get_double (x);
	char fltstr[32];
	sprintf (fltstr, "%f", value);

	if (letter == 'D' || letter == 'E')
	  {
	    int i, found = 0;
	    for (i = 0; i <= datalbl_ndx; i++)
	      if (strcmp (fltstr, datalbl[i].value) == 0)
		{
		  found = 1;
		  break;
		}
	    if (!found)
	      {
		strcpy (datalbl[i = ++datalbl_ndx].value, fltstr);
		datalbl[i].name = float_label (letter, value);
		datalbl[i].size = (letter == 'E') ? 3 : 2;
		check_section (Konst);
		fprintf (file, "K%s \tdata%s %s ;p_o\n", datalbl[i].name,
			(letter == 'E' ? "ef" : "f"), fltstr);
		check_section (Normal);
	      }
	  }
	else if (letter == 'F' || letter == 'G')
	  {
	    int i, found = 0;
	    for (i = 0; i <= datalbl_ndx; i++)
	      if (strcmp (fltstr, datalbl[i].value) == 0)
		{
		  found = 1;
		  break;
		}
	    if (!found)
	      {
		fprintf (stderr,
		   "float value %f not found upon label reference\n", value);
		strcpy (datalbl[i = ++datalbl_ndx].value, fltstr);
		datalbl[i].name = float_label (letter, value);
		datalbl[i].size = (letter == 'G') ? 3 : 2;
		check_section (Konst);
		fprintf (file, "K%s \tdata%s %s ;p_o\n", datalbl[i].name,
			(letter == 'G' ? "ef" : "f"), fltstr);
		check_section (Normal);
	      }
	    fprintf (file, "%s ;P_O 'F'", datalbl[i].name);
	  }
	else
	  fprintf (file, " %s  ;P_O cst_dbl ", fltstr);
      }
 */
      fprintf (file, "%f", get_double (x));
      break;

    case CONST_INT:
      if (letter == 'J')
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, -INTVAL (x));
      else if (letter == 'b')
        fprintf (file, "%d", which_bit (INTVAL (x)));
      else if (letter == 'B')
        fprintf (file, "%d", which_bit (~INTVAL (x)));
      else if (letter == 'w')
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) - 16);
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CODE_LABEL:
      fprintf (file, "L%d", XINT (x, 3));
      break;

    case CALL:
      fprintf (file, "CALL nargs=");
      fprintf (file, HOST_PTR_PRINTF, XEXP (x, 1));
      fprintf (file, ", func is either '%s' or '%s'",
	       XSTR (XEXP (XEXP (x, 0), 1), 0), XSTR (XEXP (x, 0), 1));
      break;

    case PLUS:
      {
	rtx op0 = XEXP (x, 0), op1 = XEXP (x, 1);
	int op0code = GET_CODE (op0), op1code = GET_CODE (op1);
	if (op1code == CONST_INT)
	  switch (op0code)
	    {
	    case REG:
	      fprintf (file, "%d,r%d  ; p_o_PLUS for REG and CONST_INT",
		       INTVAL (op1), REGNO (op0));
	      break;
	    case SYMBOL_REF:
	      fprintf (file, "%d+%s", INTVAL (op1), XSTR (op0, 0));
	      break;
	    case MEM:
	      fprintf (file, "%d,[mem:", INTVAL (op1));
	      output_address (XEXP (op0, 0));
	      fprintf (file, "] ;P_O plus");
	      break;
	    default:
	      fprintf (file, "p_o_PLUS UFO, code=%d, with CONST=%d",
		       (int) op0code, INTVAL (op1));
	    }
	else if (op1code == SYMBOL_REF && op0code == REG)
	  fprintf (file, "%s,r%d  ; P_O: (plus reg sym)",
		   XSTR (op1, 0), REGNO (op0));
	else
	  fprintf (file, "p_o_+: op0code=%d, op1code=%d", op0code, op1code);
      }
      break;

    default:
      fprintf (file, "p_o_UFO code=%d", GET_CODE (x));
    }

  addr_inc = 0;
}

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%d,r%d ; P_O_A", addr_inc, REGNO (addr));
      break;
    case PLUS:
      {
	register rtx x = XEXP (addr, 0), y = XEXP (addr, 1);
	switch (GET_CODE (x))
	  {
	  case REG:
	    switch (GET_CODE (y))
	      {
	      case CONST:
		output_address (XEXP (y, 0));
		fprintf (file, ",r%d ;P_O_A reg + const expr", REGNO (x));
		break;
	      case CONST_INT:
		fprintf (file, "%d,r%d", INTVAL (y) + addr_inc, REGNO (x));
		break;
	      case SYMBOL_REF:
		fprintf (file, "%s", XSTR (y, 0));
		if (addr_inc)
		  fprintf (file, "+%d", addr_inc);
		fprintf (file, ",r%d  ; P_O_A reg + sym", REGNO (x));
		break;
	      case LABEL_REF:
		output_address (XEXP (y, 0));
		fprintf (file, ",r%d  ; P_O_A reg + label", REGNO (x));
		break;
	      default:
		fprintf (file, "[P_O_A reg%d+UFO code=%d]",
			 REGNO (x), GET_CODE (y));
	      }
	    break;
	  case LABEL_REF:
	    output_address (XEXP (x, 0));
	    break;
	  case SYMBOL_REF:
	    switch (GET_CODE (y))
	      {
	      case CONST_INT:
		fprintf (file, "%d+%s", INTVAL (y) + addr_inc, XSTR (x, 0));
		break;
	      case REG:
		fprintf (file, "%s,r%d ;P_O_A sym + reg",
			 XSTR (x, 0), REGNO (y));
		break;
	      default:
		fprintf (file, "P_O_A sym/lab+UFO[sym=%s,code(y)=%d]",
			 XSTR (x, 0), GET_CODE (y));
	      }
	    break;
	  case CONST:
	    output_address (XEXP (x, 0));
	    if (GET_CODE (y) == REG)
	      fprintf (file, ",r%d ;P_O_A const + reg", REGNO (x));
	    else
	      fprintf (file, "P_O_A const+UFO code(y)=%d]", GET_CODE (y));
	    break;
	  case MEM:
	    output_address (y);
	    fprintf (file, ",[mem:");
	    output_address (XEXP (x, 0));
	    fprintf (file, "] ;P_O_A plus");
	    break;
	  default:
	    fprintf (file, "P_O_A plus op1_UFO[code1=%d,code2=%d]",
		     GET_CODE (x), GET_CODE (y));
	  }
      }
      break;
    case CONST_INT:
      if (INTVAL (addr) < 0x10000 && INTVAL (addr) >= -0x10000)
	fprintf (file, "%d ; p_o_a const addr?!", INTVAL (addr));
      else
	{
	  fprintf (file, "[p_o_a=ILLEGAL_CONST]");
	  output_addr_const (file, addr);
	}
      break;
    case LABEL_REF:
    case SYMBOL_REF:
      fprintf (file, "%s", XSTR (addr, 0));
      if (addr_inc)
	fprintf (file, "+%d", addr_inc);
      break;
    case MEM:
      fprintf (file, "[memUFO:");
      output_address (XEXP (addr, 0));
      fprintf (file, "]");
      break;
    case CONST:
      output_address (XEXP (addr, 0));
      fprintf (file, " ;P_O_A const");
      break;
    case CODE_LABEL:
      fprintf (file, "L%d", XINT (addr, 3));
      break;
    default:
      fprintf (file, " p_o_a UFO, code=%d val=0x%x",
	       (int) GET_CODE (addr), INTVAL (addr));
      break;
    }
  addr_inc = 0;
}


/*
 *  Return non zero if the LS 16 bits of the given value has just one bit set,
 *  otherwise return zero. Note this function may be used to detect one
 *  bit clear by inverting the param.
 */
int
one_bit_set_p (x)
     int x;
{
  x &= 0xffff; 
  return x && (x & (x - 1)) == 0;
}


/*
 * Return the number of the least significant bit set, using the  same
 * convention for bit numbering as in the MIL-STD-1750 sb instruction.
 */
static int
which_bit (x)
     int x;
{
  int b = 15;

  while (b > 0 && (x & 1) == 0)
    {
      b--;
      x >>= 1;
    }

  return b;
}


