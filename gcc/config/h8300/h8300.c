/* Subroutines for insn-output.c for Hitachi H8/300.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.

   Contributed by Steve Chamberlain (sac@cygnus.com) and
   Jim Wilson (wilson@cygnus.com).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "tree.h"

/* Forward declarations.  */
void print_operand_address ();
char *index ();

/* True if a #pragma interrupt has been seen for the current function.  */
int pragma_interrupt;

/* True if a #pragma saveall has been seen for the current function.  */
int pragma_saveall;

char *names_big[] 
 = {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"};

char *
byte_reg (x, b)
     rtx x;
     int b;
{
  static char *names_small[] 
    =  {"r0l", "r0h", "r1l", "r1h", "r2l", "r2h", "r3l", "r3h",
	"r4l", "r4h", "r5l", "r5h", "r6l", "r6h", "r7lBAD", "r7hBAD"};

  return names_small[REGNO (x) * 2 + b];
}

/* REGNO must be saved/restored across calls if this macro is true.  */
static int 
word_reg_used (regno)
int regno;
{
  if (regno < 7  
      && (pragma_interrupt || pragma_saveall 
	  || (regno == FRAME_POINTER_REGNUM && regs_ever_live[regno])
	  || (regs_ever_live[regno] & ! call_used_regs[regno])))
    return 1;
  return 0;
}

/* Output assembly language to FILE for the operation OP with operand size
   SIZE.  */
static void
dosize (file, op, size, fped)
     FILE *file;
     char *op;
     unsigned int size;
     int fped;
{
  switch (size)
    {
    case 4:
    case 3:
      fprintf (file, "\t%ss\t#%d,sp\n", op, 2);
      size -= 2;
      /* Fall through...  */
    case 2:
    case 1:
      fprintf (file, "\t%ss\t#%d,sp\n", op, size);
      size = 0;
      break;
    case 0:
      break;
    default:
      fprintf (file, "\tmov.w\t#%d,r5\n\t%s.w\tr5,sp\n", size, op);
      size = 0;
      break;
    }
}

/* Output assembly language code for the function prologue.  */
static int push_order[FIRST_PSEUDO_REGISTER] 
  = {6, 5, 4, 3, 2, 1, 0, -1, -1};
static int pop_order[FIRST_PSEUDO_REGISTER] 
  = {0, 1, 2, 3, 4, 5, 6, -1, -1};

/* This is what the stack looks like after the prolog of 
   a function with a frame has been set up:

	<pushed args>
	return pc
   fp->	old fp
   	<locals>
	<saved register-0> 
	<saved register-1> 
   sp->	<saved register-n>


   This is what the stack looks like after the prolog of
   a function which doesn't have a frame:

   	<pushed args>
   	return pc
	<locals>
	<saved register-0>
   sp-> <saved register-n>
*/

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  register int mask = 0;
  int fsize = (size + 1) & -2;
  int idx;

  if (frame_pointer_needed)
    {
      /* Push the fp.  */
      fprintf (file, "\tpush\t%s\n", names_big[FRAME_POINTER_REGNUM]);
      fprintf (file, "\tmov.w\tr7,r6\n");

      /* Leave room for the locals.  */
      dosize (file, "sub", fsize, 1);

      /* Push the rest of the registers.  */
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++) 
	{
	  int regno = push_order[idx];

	  if (regno >= 0 && word_reg_used (regno)
	      && regno != FRAME_POINTER_REGNUM)
	    fprintf (file, "\tpush\t%s\n", names_big[regno]);
	}
    }
  else
    {
      dosize (file, "sub", fsize, 0);
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  int regno = push_order[idx];

	  if (regno >= 0 && word_reg_used (regno))
	    fprintf (file, "\tpush\t%s\n", names_big[regno]);
	}
    }
}

/* Output assembly language code for the function epilogue.  */

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  register int regno;
  register int mask = 0;
  int fsize = (size + 1) & -2;
  int nregs;
  int offset;
  int idx;
  rtx insn = get_last_insn ();

  /* If the last insn was a BARRIER, we don't have to write any code.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn && GET_CODE (insn) == BARRIER)
    return;

  nregs = 0;

  if (frame_pointer_needed)
    {
      /* Pop saved registers.  */
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  regno = pop_order[idx];
	  if (regno >= 0 && regno != FRAME_POINTER_REGNUM
	      && word_reg_used (regno))
	    fprintf (file, "\tpop\t%s\n", names_big[regno]);
	}
      /* Deallocate locals.  */
      dosize (file, "add", fsize, 1);
      /* Pop frame pointer.  */
      fprintf (file, "\tpop\t%s\n", names_big[FRAME_POINTER_REGNUM]);
    }
  else
    {
      /* Deallocate locals and pop saved registers.  */
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  regno = pop_order[idx];
	  if (regno >= 0 && word_reg_used (regno))
	    fprintf (file, "\tpop\t%s\n", names_big[regno]);
	}
      dosize (file, "add", fsize, 0);
    }
  if (pragma_interrupt)
    fprintf (file, "\trte\n");
  else
    fprintf (file, "\trts\n");

  pragma_interrupt = 0;
  pragma_saveall = 0;
}

/* Return true if VALUE is a valid constant for constraint 'P'.  */

int
potl8 (value)
{
  switch (value)
    {
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
    case 32:
    case 64:
    case 128:
      return 1;
    }
  return 0;
}

/* Return true if VALUE is a valid constant for constraint 'O'.  */
int
potg8 (value)
     int value;
{
  switch (value)
    {
    case 256:
    case 512:
    case 1024:
    case 2048:
    case 4096:
    case 8192:
    case 16384:
    case 32768:
      return 1;
    }
  return 0;
}

/* Return true is OP is a valid source operand for an integer move
   instruction.  */
int
general_operand_src (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* We can't have a pre-dec as a source.  */
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == PRE_DEC)
    return 0;
  return general_operand (op, mode);
}

/* Return true if OP is a valid destination operand for an integer move
   instruction.  */
int
general_operand_dst (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* We can't have a post-inc as a dest.  */
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == POST_INC)
    return 0;
  return general_operand (op, mode);
}

/* Handle machine specific pragmas for compatibility with existing
   compilers for the H8/300

   pragma saveall generates prolog/epilog code which saves and
   restores all the registers on function entry.
   
   pragma interrupt saves and restores all registers, and exits with
   an rte instruction rather than an rts.  A pointer to a function
   with this attribute may be safely used in an interrupt vector.  */
int
handle_pragma (file)
     FILE *file;
{
  int c;
  char pbuf[20];
  int psize;

  c = getc (file);
  while (c == ' ' || c == '\t')
    c = getc (file);

  if (c == '\n' || c == EOF)
    return c;

  for (psize = 0; psize < sizeof (pbuf) - 1 && isalpha (c); psize++)
    {
      pbuf[psize] = c;
      c = getc (file);
    }

  pbuf[psize] = 0;

  if (strcmp (pbuf, "interrupt") == 0)
    pragma_interrupt = 1;

  if (strcmp (pbuf, "saveall") == 0)
    pragma_saveall = 1;

  return c;
}

/* If the next arg with MODE and TYPE is to be passed in a register, return
   the rtx to represent where it is passed.  CUM represents the state after
   the last argument.  NAMED is not used.  */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  rtx result = 0;
  int libcall = 0;

  /* Right now reload has a problem with reg passing with small reg
     classes.  */

  if (cum->libcall || (named && TARGET_QUICKCALL))
    libcall = 1;

  if (TARGET_NOQUICK)
    libcall = 0;

  if (mode != VOIDmode && libcall && mode != DFmode && mode != SFmode)
    {
      switch (cum->nbytes)
	{
	case 0:
	  result = gen_rtx (REG, mode, 0);
	  break;
	case 2:
	  result = gen_rtx (REG, mode, 1);
	  break;
	case 4:
	  result = gen_rtx (REG, mode, 4);
	  break;
	case 6:
	  result = gen_rtx (REG, mode, 5);
	  break;
	default:
	  return 0;
	}
    }
  return result;
}

/* Documentation for the machine specific operand escapes:

   'C' print (operand - 2).
   'E' low byte of reg or -ve lsb of constant
   'F' high byte of reg of -ve  msb of constant

   'G' negate constant
   'L' fake label, changed after used twice.
   'M' turn a 'M' constant into its negative mod 2.
   'T' print operand as a word
   'V' print log2 of constant - used for bset instructions
   'X' 8 bit register or other operand

   'Y' print either l or h depending on whether last 'Z' operand < 8 or >= 8.
   'Z' print int & 7

   'e' first word of 32 bit value
   'f' second word of 32 bit value

   'j' print operand as condition code.
   'k' print operand as reverse condition code.

   's' low byte of 16 bit value
   't' high byte of 16 bit value

   'w' 1st byte of 32 bit value           zzzzzzzz yyyyyyyy xxxxxxxx wwwwwwww
   'x' 2nd byte of 32 bit value
   'y' 3rd byte of 32 bit value
   'z' 4th byte of 32 bit value

 */

/* Return assembly language string which identifies a comparison type.  */

char *
cond_string (code)
     enum rtx_code code;
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      return "ge";
    case GT:
      return "gt";
    case LE:
      return "le";
    case LT:
      return "lt";
    case GEU:
      return "hs";
    case GTU:
      return "hi";
    case LEU:
      return "ls";
    case LTU:
      return "lo";
    default:
      abort ();
    }
}

/* Print operand X using operand code CODE to assembly language output file
   FILE.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  /* This is used to general unique labels for the 'L' code.  */
  static int lab = 1000;

  /* This is used for communication between the 'P' and 'U' codes.  */
  static char *last_p;

  /* This is used for communication between the 'Z' and 'Y' codes.  */
  static int bitint;

  switch (code)
    {
    case 'L':
      /* 'L' must always be used twice in a single pattern.  It generates
	 the same lable twice, and then will generate a unique label the
	 next time it is used.  */
      asm_fprintf (file, "tl%d", (lab++) / 2);
      break;

    case 'X':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", byte_reg (x, 0));
      else
	goto def;
      break;

    case 'G':
      if (GET_CODE (x) != CONST_INT)
	abort ();
      fprintf (file, "#%d", 0xff & (-INTVAL (x)));
      break;

    case 'T':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", names_big[REGNO (x)]);
      else
	goto def;
      break;

    case 'w':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", INTVAL (x) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 2));
      break;

    case 'x':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 8) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 3));
      break;

    case 'y':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 16) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 0));
      break;

    case 'z':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 24) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 1));
      break;

      /* FOR 16 bits.  */
    case 't':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 8) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 1));
      break;

    case 's':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x)) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 0));
      break;

    case 'u':
      if (GET_CODE (x) != CONST_INT)
	abort ();
      fprintf (file, "%d", INTVAL (x));
      break;

    case 'Z':
      bitint = INTVAL (x);
      fprintf (file, "#%d", bitint & 7);
      break;

    case 'Y':
      fprintf (file, "%c", bitint > 7 ? 'h' : 'l');
      break;

    case 'O':
      bitint = exact_log2 ((~INTVAL (x)) & 0xff);
      if (bitint == -1)
	abort ();
      fprintf (file, "#%d", bitint & 7);
      break;

    case 'V':
      bitint = exact_log2 (INTVAL (x));
      if (bitint == -1)
	abort ();
      fprintf (file, "#%d", bitint & 7);
      break;

    case 'P':
      if (REGNO (XEXP (XEXP (x, 0), 0)) == STACK_POINTER_REGNUM)
	{
	  last_p = "";
	  fprintf (file, ".w");
	}
      else
	{
	  last_p = "l";
	  fprintf (file, ".b");
	}
      break;

    case 'U':
      fprintf (file, "%s%s", names_big[REGNO (x)], last_p);
      break;

    case 'M':
      /* For -4 and -2, the other 2 is handled separately.  */
      switch (INTVAL (x))
	{
	case -2:
	case -4:
	  fprintf (file, "#2");
	  break;
	case -1:
	case -3:
	  fprintf (file, "#1");
	  break;

	default:
	  abort ();
	}
      break;

    case 'e':
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%s", names_big[REGNO (x)]);
	  break;
	case MEM:
	  x = adj_offsettable_operand (x, 0);
	  print_operand (file, x, 0);
	  break;
	case CONST_INT:
	  fprintf (file, "#%d", ((INTVAL (x) >> 16) & 0xffff));
	  break;
	default:
	  abort ();
	  break;
	}
      break;

    case 'f':
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%s", names_big[REGNO (x) + 1]);
	  break;

	case MEM:
	  x = adj_offsettable_operand (x, 2);
	  print_operand (file, x, 0);
	  break;

	case CONST_INT:
	  fprintf (file, "#%d", INTVAL (x) & 0xffff);
	  break;

	default:
	  abort ();
	}
      break;

    case 'C':
      fprintf (file, "#%d", INTVAL (x) - 2);
      break;

    case 'E':
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%sl", names_big[REGNO (x)]);
	  break;

	case CONST_INT:
	  fprintf (file, "#%d", (-INTVAL (x)) & 0xff);
	  break;

	default:
	  abort ();
	}
      break;

    case 'F':
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%sh", names_big[REGNO (x)]);
	  break;

	case CONST_INT:
	  fprintf (file, "#%d", ((-INTVAL (x)) & 0xff00) >> 8);
	  break;

	default:
	  abort ();
	}
      break;

    case 'j':
      asm_fprintf (file, cond_string (GET_CODE (x)));
      break;

    case 'k':
      asm_fprintf (file, cond_string (reverse_condition (GET_CODE (x))));
      break;
    def: ;
    default:
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%s", names_big[REGNO (x)]);
	  break;

	case MEM:
	  fprintf (file, "@");
	  output_address (XEXP (x, 0));
	  break;

	case CONST_INT:
	case SYMBOL_REF:
	case CONST:
	case LABEL_REF:
	  fprintf (file, "#");
	  print_operand_address (file, x);
	  break;
	}
    }
}

/* Output assembly language output for the address ADDR to FILE.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%s", names_big[REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "-%s", names_big[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "%s+", names_big[REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:
      fprintf (file, "(");
      if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  /* reg,foo */
	  print_operand_address (file, XEXP (addr, 1));
	  fprintf (file, ",");
	  print_operand_address (file, XEXP (addr, 0));
	}
      else
	{
	  /* foo+k */
	  print_operand_address (file, XEXP (addr, 0));
	  fprintf (file, "+");
	  print_operand_address (file, XEXP (addr, 1));
	}
      fprintf (file, ")");
      break;

    case CONST_INT:
      if (INTVAL (addr) < 0)
	{
	  int v = -INTVAL (addr);

	  fprintf (file, "-%d", v);
	}
      else
	fprintf (file, "%d", INTVAL (addr));
      break;

    default:
      output_addr_const (file, addr);
      break;
    }
}

/* Output to FILE a reference to the user-level label NAME.
   Strip off the section name if any.  It is separated from the label name
   by a space.  */

void
asm_output_labelref (file, name)
     FILE *file;
     char *name;
{
  char *p;

  fputc ('_', file);

  for (p = name; *p; p++)
    {
      if (*p == ' ')
	{
	  /* If we found a space in the name, then we've skipped over the
	     section encoding.  */
	  fputs (p + 1, file);
	  return;
	}
    }

  /* No space, so no section.  */
  fputs (name, file);
}

/* Output all insn addresses and their sizes into the assembly language
   output file.  This is helpful for debugging whether the length attributes
   in the md file are correct.  This is not meant to be a user selectable
   option.  */

void
final_prescan_insn (insn, operand, num_operands)
     rtx insn, *operand;
     int num_operands;
{
  /* This holds the last insn address.  */
  static int last_insn_address = 0;

  int uid = INSN_UID (insn);

  if (TARGET_ADDRESSES)
    {
      fprintf (asm_out_file, "; %d %d\n", insn_addresses[uid],
	       insn_addresses[uid] - last_insn_address);
      last_insn_address = insn_addresses[uid];
    }
}

static void
shift_n_bits (lval, operand, f, notzero)
     rtx lval;
     rtx operand;
     rtx (*f) ();
     int notzero;
{
  rtx label = gen_label_rtx ();
  rtx bot = gen_label_rtx ();

  if (! notzero)
    {
      /* Have to put a zero test at the top.  */
      emit_insn (gen_rtx (SET, VOIDmode, cc0_rtx, lval));
      emit_jump_insn (gen_beq (bot));
    }
  emit_label (label);
  f (operand);
  emit_insn (gen_rtx (SET, QImode, lval,
		      gen_rtx (MINUS, QImode, lval, const1_rtx)));
  emit_insn (gen_rtx (SET, VOIDmode, cc0_rtx, lval));
  emit_jump_insn (gen_bne (label));

  emit_label (bot);
  /* We can't end an expand with a label.  */
  emit_move_insn (operand, operand);
}

int
can_shift (code, operands, f, limit, fby_eight)
     int code;
     rtx operands[];
     rtx (*f) ();
     int limit;
     rtx (*fby_eight) ();
{
  extern int rtx_equal_function_value_matters;

  emit_move_insn (operands[0], operands[1]);

  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx lval;

      /* Can't define expand a loop after rtl generation.  */
      if (! rtx_equal_function_value_matters)
	return 0;

      lval = gen_reg_rtx (QImode);

      convert_move (lval, operands[2], 1);
      shift_n_bits (lval, operands[0], f, 0);
    }
  else
    {
      int i;

      i = INTVAL (operands[2]);
      if (i >= 8 && fby_eight)
	{
	  fby_eight (operands[0]);
	  i -= 8;
	}
      if (i > limit)
	{
	  rtx lval;

	  /* Can't define expand a loop after rtl generation.  */
	  if (! rtx_equal_function_value_matters)
	    return 0;
	  lval = gen_reg_rtx (QImode);
	  emit_move_insn (lval, gen_rtx (CONST_INT, VOIDmode, i));
	  shift_n_bits (lval, operands[0], f, 1);
	}
      else
	{
	  while (i--)
	    f (operands[0]);
	}
    }
  return 1;
}

int
domovsi (operands)
     rtx operands[];
{
  rtx src = operands[1];
  rtx dst = operands[0];

  if (push_operand (dst, GET_MODE (dst)))
    {
      /* Source must be a reg.  */
      if (! REG_P (src))
	{
	  rtx tmp = gen_reg_rtx (GET_MODE (dst));

	  emit_move_insn (tmp, src);
	  operands[1] = tmp;
	}
    }
  return 0;
}

int
io (FROM, TO)
{
  int OFFSET = 0;

  if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)
    (OFFSET) = 2 + frame_pointer_needed * 2;
  else
    {
      int regno;
      int offset = 0;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((regs_ever_live[regno]
	     && (! call_used_regs[regno] || regno == FRAME_POINTER_REGNUM)))
	  offset += 2;

      (OFFSET) = offset + get_frame_size ();

      if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)
	(OFFSET) += 2;		/* Skip saved PC.  */
    }
  return OFFSET;
}
