/* Subroutines for insn-output.c for Convex.
   Copyright (C) 1988, 1993, 1994, 1997 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "output.h"
#include "expr.h"

/* Tables used in convex.h */

char regno_ok_for_index_p_base[1 + LAST_VIRTUAL_REGISTER + 1];
enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];
enum reg_class reg_class_from_letter[256];

/* Target cpu index. */

int target_cpu;

/* Boolean to keep track of whether the current section is .text or not.
   Used by .align handler in convex.h. */

int current_section_is_text;

/* Communication between output_compare and output_condjump. */

static rtx cmp_operand0, cmp_operand1;
static char cmp_modech;

/* Forwards */

static rtx frame_argblock;
static int frame_argblock_size;
static rtx convert_arg_pushes ();
static void expand_movstr_call ();

/* Here from OVERRIDE_OPTIONS at startup.  Initialize constant tables. */

init_convex ()
{
  int regno;

  /* Set A and S reg classes. */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (A_REGNO_P (regno))
      {
	regno_ok_for_index_p[regno] = 1;
	regno_reg_class[regno] = INDEX_REGS;
      }
    else
      {
	regno_ok_for_index_p[regno] = 0;
	regno_reg_class[regno] = S_REGS;
      }

  /* Can't index off the stack pointer, register 0. */
  regno_ok_for_index_p[STACK_POINTER_REGNUM] = 0;
  regno_reg_class[STACK_POINTER_REGNUM] = SP_REGS;

  /* Can't index off aliases of the stack pointer.  */
  regno_ok_for_index_p[VIRTUAL_INCOMING_ARGS_REGNUM] = 1;
  regno_ok_for_index_p[VIRTUAL_STACK_VARS_REGNUM] = 1;
  regno_ok_for_index_p[VIRTUAL_STACK_DYNAMIC_REGNUM] = 0;
  regno_ok_for_index_p[VIRTUAL_OUTGOING_ARGS_REGNUM] = 0;

  /* Can't index off hard reg -1 == pseudos not assigned */
  regno_ok_for_index_p[-1] = 0;

  /* Set reg class letters */
  reg_class_from_letter['a'] = A_REGS;
  reg_class_from_letter['A'] = INDEX_REGS;
  reg_class_from_letter['d'] = S_REGS;

  /* Turn off floating point exception enables in the psw. */
  psw_disable_float ();
}

psw_disable_float ()
{
#if __convex__ && __GNUC__
  register int *p;
  asm ("mov fp,%0" : "=a" (p));
  while (p)
    {
      p[1] &= ~0x1000c400;
      p = (int *) p[2];
    }
#endif  
}

/* Here to output code for a compare insn.  Output nothing, just
   record the operands and their mode. */

char *
output_cmp (operand0, operand1, modech)
     rtx operand0, operand1;
     char modech;
{
  cmp_operand0 = operand0;
  cmp_operand1 = operand1;
  cmp_modech = modech;
  return "";
}

/* Output code for a conditional jump.  The preceding instruction
   is necessarily a compare.  Output two instructions, for example
       eq.w a1,a2
       jbra.t L5
   for
       (cmpsi a1 a2)
       (beq L5)
 */

char *
output_condjump (label, cond, jbr_sense)
     rtx label;
     char *cond;
     char jbr_sense;
{
  rtx operands[3];
  char cmp_op[4];
  char buf[80];
  char jbr_regch;

  strcpy (cmp_op, cond);

  /* [BL] mean the value is being compared against immediate 0.
     Use neg.x, which produces the same carry that eq.x #0 would if it
     existed.  In this case operands[1] is a scratch register, not a
     compare operand. */

  if (cmp_modech == 'B' || cmp_modech == 'L')
    {
      cmp_modech = cmp_modech - 'A' + 'a';
      strcpy (cmp_op, "neg");
    }

  /* [WH] mean the value being compared resulted from "add.[wh] #-1,rk"
     when rk was nonnegative -- we can omit equality compares against -1
     or inequality compares against 0. */

  else if (cmp_modech == 'W' || cmp_modech == 'H')
    {
      if (! strcmp (cmp_op, "eq") && cmp_operand1 == constm1_rtx)
	jbr_sense ^= 't' ^ 'f';
      else if (! strcmp (cmp_op, "lt") && cmp_operand1 == const0_rtx)
	;
      else
	cmp_modech = cmp_modech - 'A' + 'a';
    }

  /* Constant must be first; swap operands if necessary.
     If lt, le, ltu, leu are swapped, change to le, lt, leu, ltu
     and reverse the sense of the jump. */

  if (! REG_P (cmp_operand1))
    {
      operands[0] = cmp_operand1;
      operands[1] = cmp_operand0;
      if (cmp_op[0] == 'l')
	{
	  cmp_op[1] ^= 'e' ^ 't';
	  jbr_sense ^= 't' ^ 'f';
	}
    }
  else
    {
      operands[0] = cmp_operand0;
      operands[1] = cmp_operand1;
    }

  operands[2] = label;

  if (S_REG_P (operands[1]))
    jbr_regch = 's';
  else if (A_REG_P (operands[1]))
    jbr_regch = 'a';
  else
    abort ();

  if (cmp_modech == 'W' || cmp_modech == 'H')
    sprintf (buf, "jbr%c.%c %%l2", jbr_regch, jbr_sense);
  else
    sprintf (buf, "%s.%c %%0,%%1\n\tjbr%c.%c %%l2",
	     cmp_op, cmp_modech, jbr_regch, jbr_sense);
  output_asm_insn (buf, operands);
  return "";
}

/* Return 1 if OP is valid for cmpsf.
   In IEEE mode, +/- zero compares are not handled by 
     the immediate versions of eq.s and on some machines, lt.s, and le.s.  
   So disallow 0.0 as the immediate operand of xx.s compares in IEEE mode. */

int
nonmemory_cmpsf_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
#if _IEEE_FLOAT_
  if (op == CONST0_RTX (SFmode))
    return 0;
#endif

  return nonmemory_operand (op, mode);
}

/* Convex /bin/as does not like unary minus in some contexts.
   Simplify CONST addresses to remove it. */

rtx
simplify_for_convex (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case MINUS:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  PUT_CODE (x, PLUS);
	  XEXP (x, 1) = GEN_INT (- INTVAL (XEXP (x, 1)));
	}
      break;

    case CONST:
      return simplify_for_convex (XEXP (x, 0));
    }

  return x;
}

/* Routines to separate CONST_DOUBLEs into component parts. */

int
const_double_high_int (x)
     rtx x;
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return CONST_DOUBLE_LOW (x);
  else
    return CONST_DOUBLE_HIGH (x);
}

int
const_double_low_int (x)
     rtx x;
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return CONST_DOUBLE_HIGH (x);
  else
    return CONST_DOUBLE_LOW (x);
}

/* Inline block copy. */

void
expand_movstr (operands)
     rtx *operands;
{
  rtx dest = operands[0];
  rtx src = operands[1];
  int align = INTVAL (operands[3]);
  int nregs, maxsize;
  unsigned len;
  enum machine_mode mode;
  rtx reg, load, store, prev_store, prev_store_2;
  int size;

  /* Decide how many regs to use, depending on load latency, and what
     size pieces to move, depending on whether machine does unaligned
     loads and stores efficiently. */

  if (TARGET_C1)
    {
      /* ld.l latency is 4, no alignment problems. */
      nregs = 3, maxsize = 8;
    }
  else if (TARGET_C2)
    {
      /* loads are latency 2 if we avoid ld.l not at least word aligned. */
      if (align >= 4)
	nregs = 2, maxsize = 8;
      else
	nregs = 2, maxsize = 4;
    }
  else if (TARGET_C34)
    {
      /* latency is 4 if aligned, horrible if not. */
      nregs = 3, maxsize = align;
    }
  else if (TARGET_C38)
    {
      /* latency is 2 if at least word aligned, 3 or 4 if unaligned. */
      if (align >= 4)
	nregs = 2, maxsize = 8;
      else
	nregs = 3, maxsize = 8;
    }
  else
    abort ();

  /* Caller is not necessarily prepared for us to fail in this
     expansion.  So fall back by generating memcpy call here. */

  if (GET_CODE (operands[2]) != CONST_INT
      || (len = INTVAL (operands[2])) > (unsigned) 32 * maxsize)
    {
      expand_movstr_call (operands);
      return;
    }

  reg = 0;
  prev_store = prev_store_2 = 0;

  while (len > 0)
    {
      if (len >= 8 && maxsize >= 8)
	mode = DImode;
      else if (len >= 4 && maxsize >= 4)
	mode = SImode;
      else if (len >= 2 && maxsize >= 2)
	mode = HImode;
      else
	mode = QImode;

      /* If no temp pseudo to reuse, or not the right mode, make one */
      if (! reg || GET_MODE (reg) != mode)
	reg = gen_reg_rtx (mode);

      /* Get src and dest in the right mode */
      if (GET_MODE (src) != mode)
	src = change_address (src, mode, 0),
	dest = change_address (dest, mode, 0);

      /* Make load and store patterns for this piece */
      load = gen_rtx (SET, VOIDmode, reg, src);
      store = gen_rtx (SET, VOIDmode, dest, reg);

      /* Emit the load and the store from last time. 
	 When we emit a store, we can reuse its temp reg. */
      emit_insn (load);
      if (prev_store)
	{
	  reg = SET_SRC (prev_store);
	  emit_insn (prev_store);
	}
      else
	reg = 0;

      /* Queue up the store, for next time or the time after that. */
      if (nregs == 2)
	prev_store = store;
      else
	prev_store = prev_store_2, prev_store_2 = store;

      /* Advance to next piece. */
      size = GET_MODE_SIZE (mode);
      src = adj_offsettable_operand (src, size);
      dest = adj_offsettable_operand (dest, size);
      len -= size;
    }

  /* Finally, emit the last stores. */
  if (prev_store)
    emit_insn (prev_store);
  if (prev_store_2)
    emit_insn (prev_store_2);
}

static void
expand_movstr_call (operands)
     rtx *operands;
{
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
		     VOIDmode, 3,
		     XEXP (operands[0], 0), Pmode,
		     XEXP (operands[1], 0), Pmode,
		     convert_to_mode (TYPE_MODE (sizetype), operands[2],
				      TREE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
}

#if _IEEE_FLOAT_
#define MAX_FLOAT 3.4028234663852886e+38
#define MIN_FLOAT 1.1754943508222875e-38
#else
#define MAX_FLOAT 1.7014117331926443e+38
#define MIN_FLOAT 2.9387358770557188e-39
#endif

int
check_float_value (mode, dp, overflow)
     enum machine_mode mode;
     REAL_VALUE_TYPE *dp;
     int overflow;
{
  REAL_VALUE_TYPE d = *dp;

  if (overflow)
    {
      *dp = MAX_FLOAT;
      return 1;
    }

  if (mode == SFmode)
    {
      if (d > MAX_FLOAT)
	{
	  *dp = MAX_FLOAT;
	  return 1;
	}
      else if (d < -MAX_FLOAT)
	{
	  *dp = -MAX_FLOAT;
	  return 1;
	}	
      else if ((d > 0 && d < MIN_FLOAT) || (d < 0 && d > -MIN_FLOAT))
	{
	  *dp = 0.0;
	  return 1;
	}
    }

  return 0;
}

/* Output the label at the start of a function.
   Precede it with the number of formal args so debuggers will have
   some idea of how many args to print. */

void
asm_declare_function_name (file, name, decl)
    FILE *file;
    char *name;
    tree decl;
{
  tree parms;
  int nargs = list_length (DECL_ARGUMENTS (decl));

  char *p, c;
  extern char *version_string;
  static char vers[4];
  int i;
  
  p = version_string;
  for (i = 0; i < 3; ) {
    c = *p;
    if (c - '0' < (unsigned) 10)
      vers[i++] = c;
    if (c == 0 || c == ' ')
      vers[i++] = '0';
    else
      p++;
  }
  fprintf (file, "\tds.b \"g%s\"\n", vers);

  if (nargs < 100)
    fprintf (file, "\tds.b \"+%02d\\0\"\n", nargs);
  else
    fprintf (file, "\tds.b \"+00\\0\"\n");

  ASM_OUTPUT_LABEL (file, name);
}

/* Print an instruction operand X on file FILE.
   CODE is the code from the %-spec that requested printing this operand;
   if `%z3' was used to print operand 3, then CODE is 'z'. */
/* Convex codes:
    %u prints a CONST_DOUBLE's high word
    %v prints a CONST_DOUBLE's low word
    %z prints a CONST_INT shift count as a multiply operand -- viz. 1 << n.
 */

print_operand (file, x, code)
     FILE *file;
     rtx x;
     char code;
{
  long u[2];
  REAL_VALUE_TYPE d;

  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (x)]);
      break;

    case MEM:
      output_address (XEXP (x, 0));
      break;

    case CONST_DOUBLE:
      REAL_VALUE_FROM_CONST_DOUBLE (d, x);
      switch (GET_MODE (x)) {
      case DFmode:
#if 0 /* doesn't work, produces dfloats */
	REAL_VALUE_TO_TARGET_DOUBLE (d, u); 
#else
	{
	  union { double d; int i[2]; } t;
	  t.d = d;
	  u[0] = t.i[0];
	  u[1] = t.i[1];
	}
#endif
	if (code == 'u')
	  fprintf (file, "#%#x", u[0]);
	else if (code == 'v')
	  fprintf (file, "#%#x", u[1]);
	else
	  outfloat (file, d, "%.17e", "#", "");
	break;
      case SFmode:
	outfloat (file, d, "%.9e", "#", "");
	break;
      default:
	if (code == 'u')
	  fprintf (file, "#%d", CONST_DOUBLE_HIGH (x));
	else
	  fprintf (file, "#%d", CONST_DOUBLE_LOW (x));
      }
      break;

    default:
      if (code == 'z')
	{
	  if (GET_CODE (x) != CONST_INT)
	    abort ();
	  fprintf (file, "#%d", 1 << INTVAL (x));
	}
      else
	{
	  putc ('#', file);
	  output_addr_const (file, x);
	}
    }
}

/* Print a memory operand whose address is X, on file FILE. */

print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  rtx index = 0;
  rtx offset = 0;

  if (GET_CODE (addr) == MEM)
    {
      fprintf (file, "@");
      addr = XEXP (addr, 0);
    }

  switch (GET_CODE (addr))
    {
    case REG:
      index = addr;
      break;

    case PLUS:
      index = XEXP (addr, 0);
      if (REG_P (index))
	offset = XEXP (addr, 1);
      else
	{
	  offset = XEXP (addr, 0);
	  index = XEXP (addr, 1);
	  if (! REG_P (index))
	    abort ();
        }
      break;

    default:
      offset = addr;
      break;
    }

  if (offset)
    output_addr_const (file, offset);

  if (index)
    fprintf (file, "(%s)", reg_names[REGNO (index)]);
}

/* Output a float to FILE, value VALUE, format FMT, preceded by PFX
   and followed by SFX. */

outfloat (file, value, fmt, pfx, sfx)
     FILE *file;
     REAL_VALUE_TYPE value;
     char *fmt, *pfx, *sfx;
{
  char buf[64];
  fputs (pfx, file);
  REAL_VALUE_TO_DECIMAL (value, fmt, buf);
  fputs (buf, file);
  fputs (sfx, file);
}

/* Here during RTL generation of return.  If we are at the final return
   in a function, go through the function and replace pushes with stores
   into a frame arg block.  This is similar to what ACCUMULATE_OUTGOING_ARGS
   does, but we must index off the frame pointer, not the stack pointer,
   and the calling sequence does not require the arg block to be at the
   top of the stack.  */

replace_arg_pushes ()
{
  /* Doesn't work yet. */
}

/* Output the insns needed to do a call.  operands[] are
     0 - MEM, the place to call
     1 - CONST_INT, the number of bytes in the arg list
     2 - CONST_INT, the number of arguments
     3 - CONST_INT, the number of bytes to pop
     4 - address of the arg list.  
 */

char *
output_call (insn, operands)
     rtx insn, *operands;
{
  if (operands[4] == stack_pointer_rtx)
    output_asm_insn ("mov sp,ap", operands);
  else
    abort ();

  if (TARGET_ARGCOUNT)
    output_asm_insn ("pshea %a2", operands);

  output_asm_insn ("calls %0", operands);

  output_asm_insn ("ld.w 12(fp),ap", operands);

  if (operands[4] == stack_pointer_rtx && operands[3] != const0_rtx)
    output_asm_insn ("add.w %3,sp", operands);

  return "";
}


/* Here after reloading, before the second scheduling pass. */

emit_ap_optimizations ()
{
  /* Removed for now. */
}

