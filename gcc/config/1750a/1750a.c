/* Subroutines for insn-output.c for MIL-STD-1750.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by O.M.Kellogg, DASA (okellogg@salyko.cube.net).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef FILE
#include <stdio.h>
#endif

#define __datalbl
#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "expr.h"
#define HAVE_cc0
#include "conditions.h"
#include "real.h"

struct datalabel_array datalbl[DATALBL_ARRSIZ];
int datalbl_ndx = -1;
struct jumplabel_array jmplbl[JMPLBL_ARRSIZ];
int jmplbl_ndx = -1;
int label_pending = 0, program_counter = 0;
enum section current_section = Normal;
char *sectname[4] =
{"Normal", "Init", "Konst", "Static"};

int
notice_update_cc (exp)
     rtx exp;
{
  if (GET_CODE (exp) == SET)
    {
      enum rtx_code src_code = GET_CODE (SET_SRC (exp));
      /* Jumps do not alter the cc's.  */
      if (SET_DEST (exp) == pc_rtx)
	return;
      /* Moving register into memory doesn't alter the cc's.
	 It may invalidate the RTX's which we remember the cc's came from.  */
      if (GET_CODE (SET_DEST (exp)) == MEM)
	{
	  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM)
	    cc_status.value1 = 0;
	  if (cc_status.value2 && GET_CODE (cc_status.value2) == MEM)
	    cc_status.value2 = 0;
	  return;
	}
      /* Function calls clobber the cc's.  */
      else if (src_code == CALL)
	{
	  CC_STATUS_INIT;
	  return;
	}
      /* Emulated longword bit-ops leave cc's incorrect */
      else if (GET_MODE (SET_DEST (exp)) == HImode ?
	       src_code == AND || src_code == IOR ||
	       src_code == XOR || src_code == NOT : 0)
	{
	  CC_STATUS_INIT;
	  return;
	}
      /* Tests and compares set the cc's in predictable ways.  */
      else if (SET_DEST (exp) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  cc_status.value1 = SET_SRC (exp);
	  return;
	}
      /* Anything that lands in a reg will set cc_status. */
      else if (REG_P (SET_DEST (exp)))
	{
	  cc_status.flags = CC_NO_OVERFLOW;
	  cc_status.value1 = SET_SRC (exp);
	  cc_status.value2 = SET_DEST (exp);
	  return;
	}
      else
	{
	  CC_STATUS_INIT;
	}
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
     int named;
{
  int size;
  rtx result;

  if (MUST_PASS_IN_STACK (mode, type))
    return (rtx) 0;
  if (mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);
  if (cum + size < 12)
    return gen_rtx (REG, mode, cum);
  else
    return (rtx) 0;
}


#ifndef STRDUP
char *
strdup (str)
     char *str;
{
  char *p;
  if (str == NULL)
    return NULL;
  if ((p = (char *) malloc (strlen (str) + 1)) == NULL)
    {
      fprintf (stderr, "dynamic memory exhausted");
      abort ();
    }
  return strcpy (p, str);
}

#endif


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
     char code;
     double value;
{
  int i = 1;
  static char label[32];
  char *p;

  label[0] = code;
  p = label + 1;
  sprintf (p, "%lf", value);
  while (*p)
    {
      *p = (*p == '+') ? 'p' :
	(*p == '-') ? 'm' : *p;
      p++;
    }
  return strdup (label);
}


char *
movcnt_regno_adjust (rtx * op)
{
  static char outstr[40];
  int cntreg = REGNO (op[2]), cntreg_1750 = REGNO (op[0]) + 1;
  int dstreg = REGNO (op[0]), srcreg = REGNO (op[1]);

  if (cntreg == cntreg_1750)
    sprintf (outstr, "mov r%%0,r%%1");
  else if (dstreg + 1 == srcreg && srcreg == cntreg + 2)
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%%0,r%%1", cntreg, dstreg);
  else if (dstreg + 1 == srcreg && srcreg < cntreg)
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%%0,r%%1", srcreg, cntreg);
  else if (srcreg + 1 == cntreg && dstreg > cntreg)
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%%0,r%%1", srcreg, dstreg);
  else
    sprintf (outstr, "xwr r%d,r%d\n\tmov r%%0,%%1\n\txwr r%d,r%d",
	     cntreg, cntreg_1750, cntreg_1750, cntreg);
  return outstr;
}

char *
mod_regno_adjust (char *instr, rtx * op)
{
  static char outstr[40];
  char *r = (!strncmp (instr, "dvr", 3) ? "r" : "");
  int modregno_gcc = REGNO (op[3]), modregno_1750 = REGNO (op[0]) + 1;

  if (modregno_gcc == modregno_1750)
    sprintf (outstr, "%s r%%0,%s%%2", instr, r);
  else
    sprintf (outstr, "lr r%d,r%d\n\t%s r%%0,%s%%2\n\txwr r%d,r%d",
	modregno_gcc, modregno_1750, instr, r, modregno_1750, modregno_gcc);
  return outstr;
}


/* Auxiliary to `nonindirect_operand':
   Check if op is a valid memory operand for 1750A arith./logic (non-move)
   instructions. */
int
memop_valid (register rtx op)
{
  if (GET_MODE (op) != Pmode && GET_MODE (op) != VOIDmode)
    return 0;
  switch (GET_CODE (op))
    {
    case MEM:
    case MINUS:
    case MULT:
    case DIV:
      return 0;
    case PLUS:
      if (!memop_valid (XEXP (op, 0)))
	return 0;
      return memop_valid (XEXP (op, 1));
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

/* extra predicate for recog: */
int
nonindirect_operand (register rtx op, enum machine_mode mode)
{
  int retval;

  switch (GET_CODE (op))
    {
    case MEM:
      retval = memop_valid (XEXP (op, 0));
      return retval;
    case REG:
      return 1;
    default:
      if (!CONSTANT_P (op))
	return 0;
    }
  return 1;
}

/* predicate for the STC instruction: */
int
small_nonneg_const (register rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT && INTVAL (op) >= 0 && INTVAL (op) <= 15)
    return 1;
  return 0;
}

/* Decide whether to output a conditional jump as a "Jump Conditional"
   or as a "Branch Conditional": */

int
find_jmplbl (int labelnum)
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

char *
branch_or_jump (char *condition, int targetlabel_number)
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



/* The PRINT_OPERAND and PRINT_OPERAND_ADDRESS macros have been
   made functions: */

print_operand (file, x, kode)
     FILE *file;
     rtx x;
     enum rtx_code kode;
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "%d", REGNO (x));
      break;
    case SYMBOL_REF:
      fprintf (file, "%s", XSTR (x, 0));
      break;
    case LABEL_REF:
    case CONST:
    case MEM:
      output_address (XEXP (x, 0));
      break;
    case CONST_DOUBLE:
/*    {
	double value = get_double (x);
	char fltstr[32];
	sprintf (fltstr, "%lf", value);

	if (kode == 'D' || kode == 'E')
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
		datalbl[i].name = float_label (kode, value);
		datalbl[i].size = (kode == 'E') ? 3 : 2;
		check_section (Konst);
		fprintf (file, "K%s \tdata%s %s ;p_o\n", datalbl[i].name,
			(kode == 'E' ? "ef" : "f"), fltstr);
		check_section (Normal);
	      }
	  }
	else if (kode == 'F' || kode == 'G')
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
		   "float value %lfnot found upon label reference\n", value);
		strcpy (datalbl[i = ++datalbl_ndx].value, fltstr);
		datalbl[i].name = float_label (kode, value);
		datalbl[i].size = (kode == 'G') ? 3 : 2;
		check_section (Konst);
		fprintf (file, "K%s \tdata%s %s ;p_o\n", datalbl[i].name,
			(kode == 'G' ? "ef" : "f"), fltstr);
		check_section (Normal);
	      }
	    fprintf (file, "%s ;P_O 'F'", datalbl[i].name);
	  }
	else
	  fprintf (file, " %s  ;P_O cst_dbl ", fltstr);
      }
 */
      fprintf (file, "%lf", get_double (x));
      break;
    case CONST_INT:
      if (kode == 'J')
	fprintf (file, "%d", -INTVAL (x));
      else if (INTVAL (x) > 0x7FFF)
	fprintf (file, "%d  ; range correction (val>0x7FFF) applied",
		 INTVAL (x) - 0x10000);
      else
	fprintf (file, "%d", INTVAL (x));
      break;
    case CODE_LABEL:
      fprintf (file, "L%d", XINT (x, 3));
      break;
    case CALL:
      fprintf (file, "CALL nargs=%d, func is either '%s' or '%s'",
       XEXP (x, 1), XSTR (XEXP (XEXP (x, 0), 1), 0), XSTR (XEXP (x, 0), 1));
      break;
    case PLUS:
      {
	rtx op0 = XEXP (x, 0), op1 = XEXP (x, 1);
	int op0code = GET_CODE (op0), op1code = GET_CODE (op1);
	if (op1code == CONST_INT)
	  switch (op0code)
	    {
	    case REG:
	      fprintf (file, "%d,r%d  ; p_o_PLUS for REG and CONST",
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
}

print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "0,r%d ; P_O_A", REGNO (addr));
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
		fprintf (file, "%d,r%d", INTVAL (y), REGNO (x));
		break;
	      case SYMBOL_REF:
		fprintf (file, "%s,r%d  ; P_O_A reg + sym",
			 XSTR (y, 0), REGNO (x));
		break;
	      default:
		fprintf (file, "[P_O_A reg%d+UFO code=%d]",
			 REGNO (x), GET_CODE (y));
	      }
	    break;
	  case LABEL_REF:
	  case SYMBOL_REF:
	    switch (GET_CODE (y))
	      {
	      case CONST_INT:
		fprintf (file, "%d+%s", INTVAL (y), XSTR (x, 0));
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
    default:
      fprintf (file, " p_o_a UFO, code=%d val=0x%x",
	       (int) GET_CODE (addr), INTVAL (addr));
      break;
    }
}


/*
ASM_FILE_END(file)
  FILE *file;
{
      if (datalbl_ndx >= 0) {
         int i, cum_size=0;
         fprintf(file,"\n\tstatic\ninit_srel\n");
         for (i = 0; i <= datalbl_ndx; i++) {
	   if (datalbl[i].name == NULL)	
	   {
	     fprintf (stderr, "asm_file_end intern err (datalbl)\n");
	     exit (0);
	   }
           fprintf(file,"%s\t block %d\n",
                 datalbl[i].name,datalbl[i].size);
           cum_size += datalbl[i].size;
	 }
         fprintf(file,"\n\tinit\n");
         fprintf(file,"\tLIM  R0,init_srel ;dst\n");
         fprintf(file,"\tLIM  R1,%d ;cnt\n",cum_size);
         fprintf(file,"\tLIM  R2,K%s ;src\n",datalbl[0].name);
         fprintf(file,"\tMOV  R0,R2\n");
         fprintf(file,"\n\tnormal\n");
         datalbl_ndx = -1;
         for (i = 0; i < DATALBL_ARRSIZ; i++)
            datalbl[i].size = 0;
      }	
      fprintf(file,"\n\tend\n");
}
 */
