/* Subroutines for insn-output.c for Hitachi H8/300.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

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

/* CPU_TYPE, says what cpu we're compiling for.  */
int cpu_type;

/* True if a #pragma interrupt has been seen for the current function.  */
int pragma_interrupt;

/* True if a #pragma saveall has been seen for the current function.  */
int pragma_saveall;

static char *names_big[] =
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"};

static char *names_extended[] =
{"er0", "er1", "er2", "er3", "er4", "er5", "er6", "er7"};

static char *names_upper_extended[] =
{"e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7"};

/* Points to one of the above.  */
/* ??? The above could be put in an array indexed by CPU_TYPE.  */
char **h8_reg_names;

/* Various operations needed by the following, indexed by CPU_TYPE.  */
/* ??? The h8/300 assembler doesn't understand pop.w (yet).  */

static char *h8_push_ops[2] =
{"push", "push.l"};
static char *h8_pop_ops[2] =
{"pop", "pop.l"};
static char *h8_mov_ops[2] =
{"mov.w", "mov.l"};

char *h8_push_op, *h8_pop_op, *h8_mov_op;

/* Initialize various cpu specific globals at start up.  */

void
h8300_init_once ()
{
  if (TARGET_H8300)
    {
      cpu_type = (int) CPU_H8300;
      h8_reg_names = names_big;
    }
  else
    {
      cpu_type = (int) CPU_H8300H;
      h8_reg_names = names_extended;
    }
  h8_push_op = h8_push_ops[cpu_type];
  h8_pop_op = h8_pop_ops[cpu_type];
  h8_mov_op = h8_mov_ops[cpu_type];
}

char *
byte_reg (x, b)
     rtx x;
     int b;
{
  static char *names_small[] =
  {"r0l", "r0h", "r1l", "r1h", "r2l", "r2h", "r3l", "r3h",
   "r4l", "r4h", "r5l", "r5h", "r6l", "r6h", "r7lBAD", "r7hBAD"};

  return names_small[REGNO (x) * 2 + b];
}

/* REGNO must be saved/restored across calls if this macro is true.  */

#define WORD_REG_USED(regno)					\
  (regno < 7 &&							\
   (pragma_interrupt						\
    || pragma_saveall						\
    || (regno == FRAME_POINTER_REGNUM && regs_ever_live[regno])	\
    || (regs_ever_live[regno] & !call_used_regs[regno])))

/* Output assembly language to FILE for the operation OP with operand size
   SIZE to adjust the stack pointer.  */
/* ??? FPED is currently unused.  */

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
      /* ??? TARGET_H8300H can do this in one insn.  */
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
      if (TARGET_H8300)
	fprintf (file, "\tmov.w\t#%d,r3\n\t%s.w\tr3,sp\n", size, op);
      else
	fprintf (file, "\t%s\t#%d,sp\n", op, size);
      size = 0;
      break;
    }
}

/* Output assembly language code for the function prologue.  */
static int push_order[FIRST_PSEUDO_REGISTER] =
{6, 5, 4, 3, 2, 1, 0, -1, -1};
static int pop_order[FIRST_PSEUDO_REGISTER] =
{0, 1, 2, 3, 4, 5, 6, -1, -1};

/* This is what the stack looks like after the prolog of 
   a function with a frame has been set up:

   <args>
   PC
   FP			<- fp
   <locals>
   <saved registers> 	<- sp

   This is what the stack looks like after the prolog of
   a function which doesn't have a frame:

   <args>
   PC
   <locals>
   <saved registers>   	<- sp
*/

int current_function_anonymous_args;

/* Extra arguments to pop, in words (IE: 2 bytes for 300, 4 for 300h */
static int extra_pop;

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  register int mask = 0;
  int fsize = (size + STACK_BOUNDARY / 8 - 1) & -STACK_BOUNDARY / 8;
  int idx;
  extra_pop = 0;

  if (current_function_anonymous_args && TARGET_QUICKCALL)
    {
      /* Push regs as if done by caller, and move around return address.  */

      switch (current_function_args_info.nbytes / UNITS_PER_WORD)
	{
	case 0:
	  /* get ret addr */
	  fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[3]);
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[2]);
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[1]);
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[0]);
	  /* push it again */
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[3]);
	  extra_pop = 3;
	  break;
	case 1:
	  /* get ret addr */
	  fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[3]);
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[2]);
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[1]);
	  /* push it again */
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[3]);
	  extra_pop = 2;
	  break;
	case 2:
	  /* get ret addr */
	  fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[3]);
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[2]);
	  /* push it again */
	  fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[3]);
	  extra_pop = 1;
	  break;
	default:
	  fprintf (file, "; varargs\n");
	  break;
	}
    }

  if (frame_pointer_needed)
    {
      /* Push fp */
      fprintf (file, "\t%s\t%s\n", h8_push_op,
	       h8_reg_names[FRAME_POINTER_REGNUM]);
      fprintf (file, "\t%s\t%s,%s\n", h8_mov_op,
	       h8_reg_names[STACK_POINTER_REGNUM],
	       h8_reg_names[FRAME_POINTER_REGNUM]);

      /* leave room for locals */
      dosize (file, "sub", fsize, 1);

      /* Push the rest of the registers */
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  int regno = push_order[idx];

	  if (regno >= 0 && WORD_REG_USED (regno) && regno != FRAME_POINTER_REGNUM)
	    fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[regno]);
	}
    }
  else
    {
      dosize (file, "sub", fsize, 0);
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  int regno = push_order[idx];

	  if (regno >= 0 && WORD_REG_USED (regno))
	    fprintf (file, "\t%s\t%s\n", h8_push_op, h8_reg_names[regno]);
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
  int fsize = (size + STACK_BOUNDARY / 8 - 1) & -STACK_BOUNDARY / 8;
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
      /* Pop saved registers */
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  regno = pop_order[idx];
	  if (regno >= 0 && regno != FRAME_POINTER_REGNUM && WORD_REG_USED (regno))
	    fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[regno]);
	}
      /* deallocate locals */
      dosize (file, "add", fsize, 1);
      /* pop frame pointer */
      fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[FRAME_POINTER_REGNUM]);
    }
  else
    {
      /* pop saved registers */
      for (idx = 0; idx < FIRST_PSEUDO_REGISTER; idx++)
	{
	  regno = pop_order[idx];
	  if (regno >= 0 && WORD_REG_USED (regno))
	    fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[regno]);
	}
      /* deallocate locals */
      dosize (file, "add", fsize, 0);
    }

  if (extra_pop)
    {
      fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[3]);
      while (extra_pop)
	{
	  fprintf (file, "\t%s\t%s\n", h8_pop_op, h8_reg_names[2]);
	  extra_pop--;
	}
      fprintf (file, "\tjmp	@%s\n", h8_reg_names[3]);
    }
  else
    {
      if (pragma_interrupt)
	fprintf (file, "\trte\n");
      else
	fprintf (file, "\trts\n");
    }

  pragma_interrupt = 0;
  pragma_saveall = 0;

  current_function_anonymous_args = 0;
}

/* Output assembly code for the start of the file.  */

asm_file_start (file)
     FILE *file;
{
  fprintf (file, ";\tGCC For the Hitachi H8/300\n");
  fprintf (file, ";\tBy Hitachi America Ltd and Cygnus Support\n");
  fprintf (file, ";\trelease F-1\n");
  if (optimize)
    fprintf (file, "; -O%d\n", optimize);
  if (TARGET_H8300H)
    fprintf (file, "\n\t.h8300h\n");
  else
    fprintf (file, "\n\n");
  output_file_directive (file, main_input_filename);
}

/* Output assembly language code for the end of file.  */

void
asm_file_end (file)
     FILE *file;
{
  fprintf (file, "\t.end\n");
}

/* Return true if VALUE is a valid constant for constraint 'P'.
   IE: VALUE is a power of two <= 2**15.  */

int
small_power_of_two (value)
     int value;
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

/* Return true if VALUE is a valid constant for constraint 'O', which
   means that the constant would be ok to use as a bit for a bclr
   instruction.  */

int
ok_for_bclr (value)
     int value;
{
  return small_power_of_two ((~value) & 0xff);
}

/* Return true is OP is a valid source operand for an integer move
   instruction.  */

int
general_operand_src (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == POST_INC)
    return 1;
  return general_operand (op, mode);
}

/* Return true if OP is a valid destination operand for an integer move
   instruction.  */

int
general_operand_dst (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == PRE_DEC)
    return 1;
  return general_operand (op, mode);
}

/* Return true if OP is a const valid for a bit clear instruction.  */

int
o_operand (operand, mode)
     rtx operand;
     enum machine_mode mode;
{
  return (GET_CODE (operand) == CONST_INT
	  && CONST_OK_FOR_O (INTVAL (operand)));
}

/* Return true if OP is a const valid for a bit set or bit xor instruction.  */

int
p_operand (operand, mode)
     rtx operand;
     enum machine_mode mode;
{
  return (GET_CODE (operand) == CONST_INT
	  && CONST_OK_FOR_P (INTVAL (operand)));
}

/* Return true if OP is a valid call operand.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (register_operand (inside, Pmode))
	return 1;
      if (CONSTANT_ADDRESS_P (inside))
	return 1;
    }
  return 0;
}

/* Return true if OP is a valid jump operand.  */

int
jump_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG)
    return mode == Pmode;

  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);
      if (register_operand (inside, Pmode))
	return 1;
      if (CONSTANT_ADDRESS_P (inside))
	return 1;
    }
  return 0;
}

/* Recognize valid operands for bitfield instructions.  */

extern int rtx_equal_function_value_matters;

int
bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* We can except any general operand, expept that MEM operands must
     be limited to those that use addresses valid for the 'U' constraint.  */
  if (!general_operand (op, mode))
    return 0;

  /* Accept any mem during RTL generation.  Otherwise, the code that does
     insv and extzv will think that we can not handle memory.  However,
     to avoid reload problems, we only accept 'U' MEM operands after RTL
     generation.  This means that any named pattern which uses this predicate
     must force its operands to match 'U' before emitting RTL.  */

  if (GET_CODE (op) == REG)
    return 1;
  if (GET_CODE (op) == SUBREG)
    return 1;
  if (!rtx_equal_function_value_matters)
    {
      /* We're building rtl */
      return GET_CODE (op) == MEM;
    }
  else
    {
      return (GET_CODE (op) == MEM
	      && EXTRA_CONSTRAINT (op, 'U'));
    }
}

/* Recognize valid operators for bit test.  */

int
eq_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  return (GET_CODE (x) == EQ || GET_CODE (x) == NE);
}

/* Handle machine specific pragmas for compatibility with existing
   compilers for the H8/300.

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
  int psize = 0;

  c = getc (file);
  while (c == ' ' || c == '\t')
    c = getc (file);

  if (c == '\n' || c == EOF)
    return c;

  /* The only pragmas we understand are interrupt and saveall.  */
  while (psize < sizeof (pbuf) - 1
	 && isalpha (c))
    {
      pbuf[psize++] = c;
      c = getc (file);
    }
  pbuf[psize] = 0;

  if (strcmp (pbuf, "interrupt") == 0)
    pragma_interrupt = 1;

  if (strcmp (pbuf, "saveall") == 0)
    pragma_saveall = 1;

  /* ??? This is deprecated.  Delete for gcc 2.8.  */
  if (strcmp (pbuf, "section") == 0)
    {
      static int printed_p = 0;
      if (!printed_p)
	{
	  warning ("#pragma section is deprecated, use section attributes");
	  printed_p = 1;
	}
      while (c && !isalpha (c))
	c = getc (file);
      psize = 0;
      while (psize < sizeof (pbuf) - 1
	     && isalpha (c) || isdigit (c) || c == '_')
	{
	  pbuf[psize++] = c;
	  c = getc (file);
	}
      pbuf[psize] = 0;
      named_section (NULL_TREE, pbuf);
    }
  ungetc (c, file);
  return c;
}

/* If the next arg with MODE and TYPE is to be passed in a register, return
   the rtx to represent where it is passed.  CUM represents the state after
   the last argument.  NAMED is not used.  */

static char *hand_list[] =
{
  "__main",
  "__cmpsi2",
  "__divhi3",
  "__modhi3",
  "__udivhi3",
  "__umodhi3",
  "__divsi3",
  "__modsi3",
  "__udivsi3",
  "__umodsi3",
  "__mulhi3",
  "__mulsi3",
  "__reg_memcpy",
  "__reg_memset",
  "__ucmpsi2",
  0,
};

/* Return an RTX to represent where a value with mode MODE will be returned
   from a function.  If the result is 0, the argument is pushed.  */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  rtx result = 0;
  char *fname;
  int regpass = 0;

  /* Pass 3 regs worth of data in regs when user asked on the command line.  */
  if (TARGET_QUICKCALL)
    regpass = 3;

  /* If calling hand written assembler, use 4 regs of args.  */

  if (cum->libcall)
    {
      char **p;

      fname = XSTR (cum->libcall, 0);

      /* See if this libcall is one of the hand coded ones.  */

      for (p = hand_list; *p && strcmp (*p, fname) != 0; p++)
	;

      if (*p)
	regpass = 4;
    }

  if (regpass)
    {
      int size;

      if (mode == BLKmode)
	size = int_size_in_bytes (type);
      else
	size = GET_MODE_SIZE (mode);

      if (size + cum->nbytes > regpass * UNITS_PER_WORD)
	{
	  result = 0;
	}
      else
	{
	  switch (cum->nbytes / UNITS_PER_WORD)
	    {
	    case 0:
	      result = gen_rtx (REG, mode, 0);
	      break;
	    case 1:
	      result = gen_rtx (REG, mode, 1);
	      break;
	    case 2:
	      result = gen_rtx (REG, mode, 2);
	      break;
	    case 3:
	      result = gen_rtx (REG, mode, 3);
	      break;
	    default:
	      result = 0;
	    }
	}
    }

  return result;
}

/* Return the cost of the rtx R with code CODE.  */

int
const_costs (r, c)
     rtx r;
     enum rtx_code c;
{
  switch (c)
    {
    case CONST_INT:
      switch (INTVAL (r))
	{
	case 0:
	case 1:
	case 2:
	case -1:
	case -2:
	  return 0;
	default:
	  return 1;
	}

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      return 3;

    case CONST_DOUBLE:
      return 20;

    default:
      return 4;
    }
}

/* Documentation for the machine specific operand escapes:

   'A' print rn in h8/300 mode, erN in H8/300H mode
   'C' print (operand - 2).
   'E' like s but negative.
   'F' like t but negative.
   'G' constant just the negative
   'L' fake label, changed after used twice.
   'M' turn a 'M' constant into its negative mod 2.
   'P' if operand is incing/decing sp, print .w, otherwise .b.
   'S' print operand as a long word
   'T' print operand as a word
   'U' if operand is incing/decing sp, print l, otherwise nothing.
   'V' find the set bit, and print its number.
   'W' find the clear bit, and print its number.
   'X' print operand as a byte
   'Y' print either l or h depending on whether last 'Z' operand < 8 or >= 8.
   'Z' print int & 7.
   'b' print the bit opcode
   'c' print the ibit opcode
   'd' bcc if EQ, bcs if NE
   'e' first word of 32 bit value - if reg, then least reg. if mem
       then least. if const then most sig word
   'f' second word of 32 bit value - if reg, then biggest reg. if mem
       then +2. if const then least sig word
   'g' bcs if EQ, bcc if NE
   'j' print operand as condition code.
   'k' print operand as reverse condition code.
   's' print as low byte of 16 bit value
   't' print as high byte of 16 bit value
   'w' print as low byte of 32 bit value
   'x' print as 2nd byte of 32 bit value
   'y' print as 3rd byte of 32 bit value
   'z' print as msb of 32 bit value
*/

/* Return assembly language string which identifies a comparison type.  */

static char *
cond_string (code)
     enum rtx_code code;
{
  switch (code)
    {
    case NE:
      if (cc_prev_status.flags & CC_DONE_CBIT)
	return "cs";
      return "ne";
    case EQ:
      if (cc_prev_status.flags & CC_DONE_CBIT)
	return "cc";
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
  /* ??? 'V' and 'W' use it too.  */
  static int bitint;

  switch (code)
    {
    case 'A':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", h8_reg_names[REGNO (x)]);
      else
	goto def;
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
    case 'G':
      if (GET_CODE (x) != CONST_INT)
	abort ();
      fprintf (file, "#%d", 0xff & (-INTVAL (x)));
      break;
    case 'L':
      /* 'L' must always be used twice in a single pattern.  It generates
	 the same label twice, and then will generate a unique label the
	 next time it is used.  */
      asm_fprintf (file, "tl%d", (lab++) / 2);
      break;
    case 'M':
      /* For 3/-3 and 4/-4, the other 2 is handled separately.  */
      switch (INTVAL (x))
	{
	case 2:
	case 4:
	case -2:
	case -4:
	  fprintf (file, "#2");
	  break;
	case 1:
	case 3:
	case -1:
	case -3:
	  fprintf (file, "#1");
	  break;
	default:
	  abort ();
	}
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
    case 'S':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", names_extended[REGNO (x)]);
      else
	goto def;
      break;
    case 'T':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", names_big[REGNO (x)]);
      else
	goto def;
      break;
    case 'U':
      fprintf (file, "%s%s", names_big[REGNO (x)], last_p);
      break;
    case 'V':
      bitint = exact_log2 (INTVAL (x));
      if (bitint == -1)
	abort ();
      fprintf (file, "#%d", bitint & 7);
      break;
    case 'W':
      bitint = exact_log2 ((~INTVAL (x)) & 0xff);
      if (bitint == -1)
	abort ();
      fprintf (file, "#%d", bitint & 7);
      break;
    case 'X':
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", byte_reg (x, 0));
      else
	goto def;
      break;
    case 'Y':
      if (bitint == -1)
	abort ();
      if (GET_CODE (x) == REG)
	fprintf (file, "%s%c", names_big[REGNO (x)], bitint > 7 ? 'h' : 'l');
      else
	print_operand (file, x, 0);
      bitint = -1;
      break;
    case 'Z':
      bitint = INTVAL (x);
      fprintf (file, "#%d", bitint & 7);
      break;
    case 'b':
      switch (GET_CODE (x))
	{
	case IOR:
	  fprintf (file, "bor");
	  break;
	case XOR:
	  fprintf (file, "bxor");
	  break;
	case AND:
	  fprintf (file, "band");
	  break;
	}
      break;
    case 'c':
      switch (GET_CODE (x))
	{
	case IOR:
	  fprintf (file, "bior");
	  break;
	case XOR:
	  fprintf (file, "bixor");
	  break;
	case AND:
	  fprintf (file, "biand");
	  break;
	}
      break;
    case 'd':
      switch (GET_CODE (x))
	{
	case EQ:
	  fprintf (file, "bcc");
	  break;
	case NE:
	  fprintf (file, "bcs");
	  break;
	default:
	  abort ();
	}
      break;
    case 'e':
      switch (GET_CODE (x))
	{
	case REG:
	  if (TARGET_H8300)
	    fprintf (file, "%s", names_big[REGNO (x)]);
	  else
	    fprintf (file, "%s", names_upper_extended[REGNO (x)]);
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
	  if (TARGET_H8300)
	    fprintf (file, "%s", names_big[REGNO (x) + 1]);
	  else
	    fprintf (file, "%s", names_big[REGNO (x)]);
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
    case 'g':
      switch (GET_CODE (x))
	{
	case NE:
	  fprintf (file, "bcc");
	  break;
	case EQ:
	  fprintf (file, "bcs");
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
    case 's':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x)) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 0));
      break;
    case 't':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 8) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, 1));
      break;
    case 'u':
      if (GET_CODE (x) != CONST_INT)
	abort ();
      fprintf (file, "%d", INTVAL (x));
      break;
    case 'w':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", INTVAL (x) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, TARGET_H8300 ? 2 : 0));
      break;
    case 'x':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "#%d", (INTVAL (x) >> 8) & 0xff);
      else
	fprintf (file, "%s", byte_reg (x, TARGET_H8300 ? 3 : 1));
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

    default:
    def:
      switch (GET_CODE (x))
	{
	case REG:
	  switch (GET_MODE (x))
	    {
	    case QImode:
#if 0				/* Is it asm ("mov.b %0,r2l", ...) */
	      fprintf (file, "%s", byte_reg (x, 0));
#else /* ... or is it asm ("mov.b %0l,r2l", ...) */
	      fprintf (file, "%s", names_big[REGNO (x)]);
#endif
	      break;
	    case HImode:
	      fprintf (file, "%s", names_big[REGNO (x)]);
	      break;
	    case SImode:
	    case SFmode:
	      fprintf (file, "%s", names_extended[REGNO (x)]);
	      break;
	    default:
	      abort ();
	    }
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
      fprintf (file, "%s", h8_reg_names[REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "-%s", h8_reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "%s+", h8_reg_names[REGNO (XEXP (addr, 0))]);
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
      {
	/* Since the h8/300 only has 16 bit pointers, negative values are also
	   those >= 32768.  This happens for example with pointer minus a
	   constant.  We don't want to turn (char *p - 2) into
	   (char *p + 65534) because loop unrolling can build upon this
	   (IE: char *p + 131068).  */
	int n = INTVAL (addr);
	if (TARGET_H8300)
	  n = (int) (short) n;
	if (n < 0)
	  /* ??? Why the special case for -ve values? */
	  fprintf (file, "-%d", -n);
	else
	  fprintf (file, "%d", n);
	break;
      }

    default:
      output_addr_const (file, addr);
      break;
    }
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

  if (TARGET_RTL_DUMP)
    {
      fprintf (asm_out_file, "\n****************");
      print_rtl (asm_out_file, PATTERN (insn));
      fprintf (asm_out_file, "\n");
    }

  if (TARGET_ADDRESSES)
    {
      fprintf (asm_out_file, "; 0x%x %d\n", insn_addresses[uid],
	       insn_addresses[uid] - last_insn_address);
      last_insn_address = insn_addresses[uid];
    }
}

/* Prepare for an SI sized move.  */

int
do_movsi (operands)
     rtx operands[];
{
  rtx src = operands[1];
  rtx dst = operands[0];
  if (!reload_in_progress && !reload_completed)
    {
      if (!register_operand (dst, GET_MODE (dst)))
	{
	  rtx tmp = gen_reg_rtx (GET_MODE (dst));
	  emit_move_insn (tmp, src);
	  operands[1] = tmp;
	}
    }
  return 0;
}

/* Function for INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET).
   Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

int
initial_offset (from, to)
{
  int offset = 0;

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    offset = UNITS_PER_WORD + frame_pointer_needed * UNITS_PER_WORD;
  else
    {
      int regno;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((regs_ever_live[regno]
	     && (!call_used_regs[regno] || regno == FRAME_POINTER_REGNUM)))
	  offset += UNITS_PER_WORD;

      /* See the comments for get_frame_size.  We need to round it up to
	 STACK_BOUNDARY.  */

      offset += ((get_frame_size () + STACK_BOUNDARY / BITS_PER_UNIT - 1)
		 & ~(STACK_BOUNDARY / BITS_PER_UNIT - 1));

      if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
	offset += UNITS_PER_WORD;	/* Skip saved PC */
    }
  return offset;
}

/* Update the condition code from the insn.  */

int
notice_update_cc (body, insn)
     rtx body;
     rtx insn;
{
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect the CC at all */
      break;

    case CC_NONE_0HIT:
      /* Insn does not change the CC, but the 0't operand has been changed.  */

      if (cc_status.value1 != 0
	  && reg_overlap_mentioned_p (recog_operand[0], cc_status.value1))
	cc_status.value1 = 0;

      if (cc_status.value2 != 0
	  && reg_overlap_mentioned_p (recog_operand[0], cc_status.value2))
	cc_status.value2 = 0;

      break;

    case CC_SET:
      /* Insn sets CC to recog_operand[0], but overflow is impossible.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_OVERFLOW;
      cc_status.value1 = recog_operand[0];
      break;

    case CC_COMPARE:
      /* The insn is a compare instruction */
      CC_STATUS_INIT;
      cc_status.value1 = SET_SRC (body);
      break;

    case CC_CBIT:
      CC_STATUS_INIT;
      cc_status.flags |= CC_DONE_CBIT;
      cc_status.value1 = 0;
      break;

    case CC_WHOOPS:
    case CC_CLOBBER:
      /* Insn clobbers CC. */
      CC_STATUS_INIT;
      break;
    }
}

/* Recognize valid operators for bit instructions */

int
bit_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  return (code == XOR
	  || code == AND
	  || code == IOR);
}

/* Shifts.

   We devote a fair bit of code to getting efficient shifts since we can only
   shift one bit at a time.  See the .md file for more comments.

   Here are some thoughts on what the absolutely positively best code is.
   "Best" here means some rational trade-off between code size and speed,
   where speed is more preferred but not at the expense of generating 20 insns.

   H8/300 QImode shifts
   1-4   - do them inline
   5-6   - ASHIFT | LSHIFTRT: rotate, mask off other bits
           ASHIFTRT: loop
   7     - ASHIFT | LSHIFTRT: rotate, mask off other bits
           ASHIFTRT: shll, subx (propagate carry bit to all bits)

   H8/300 HImode shifts
   1-4   - do them inline
   5-6   - loop
   7     - shift other way once, move byte into place, move carry bit into place
   8     - move byte, zero (ASHIFT | LSHIFTRT) or sign extend other (ASHIFTRT)
   9     - inline shift 1-4, move byte, set other byte
   13-14 - ASHIFT | LSHIFTRT: rotate 3/2, mask, move byte, set other byte to 0
         - ASHIFTRT: loop
   15    - ASHIFT | LSHIFTRT: rotate 1, mask, move byte, set other byte to 0
         - ASHIFTRT: shll, subx, set other byte

   H8/300 SImode shifts
   1-2   - do them inline
   3-6   - loop
   7     - shift other way once, move bytes into place,
           move carry into place (possibly with sign extension)
   8     - move bytes into place, zero or sign extend other
   9-14  - loop
   15    - shift other way once, move word into place, move carry into place
   16    - move word, zero or sign extend other
   17-23 - loop
   24    - move bytes into place, zero or sign extend other
   25-27 - loop
   28-30 - ASHIFT | LSHIFTRT: rotate top byte, mask, move byte into place,
                              zero others
           ASHIFTRT: loop
   31    - ASHIFT | LSHIFTRT: rotate top byte, mask, byte byte into place,
                              zero others
           ASHIFTRT: shll top byte, subx, copy to other bytes

   H8/300H QImode shifts
   - same as H8/300

   H8/300H HImode shifts
   - same as H8/300

   H8/300H SImode shifts
   (These are complicated by the fact that we don't have byte level access to
   the top word.)
   A word is: bytes 3,2,1,0 (msb -> lsb), word 1,0 (msw -> lsw)
   1-4   - do them inline
   5-14  - loop
   15    - shift other way once, move word into place, move carry into place
           (with sign extension for ASHIFTRT)
   16    - move word into place, zero or sign extend other
   17-23 - loop
   24    - ASHIFT: move byte 0(msb) to byte 1, zero byte 0,
                   move word 0 to word 1, zero word 0
           LSHIFTRT: move word 1 to word 0, move byte 1 to byte 0,
                     zero word 1, zero byte 1
           ASHIFTRT: move word 1 to word 0, move byte 1 to byte 0,
                     sign extend byte 0, sign extend word 0
   25-27 - either loop, or
           do 24 bit shift, inline rest
   28-30 - ASHIFT: rotate 4/3/2, mask
           LSHIFTRT: rotate 4/3/2, mask
           ASHIFTRT: loop
   31    - shll, subx byte 0, sign extend byte 0, sign extend word 0

   Don't Panic!!!

   All of these haven't been implemented.  I've just documented them and
   provided hooks so they can be.
*/

int
nshift_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  switch (GET_CODE (x))
    {
    case ASHIFTRT:
    case LSHIFTRT:
    case ASHIFT:
      return 1;

    default:
      return 0;
    }
}

/* Called from the .md file to emit code to do shifts.
   Returns a boolean indicating success
   (currently this is always TRUE).  */

int
expand_a_shift (mode, code, operands)
     enum machine_mode mode;
     int code;
     rtx operands[];
{
  extern int rtx_equal_function_value_matters;

  emit_move_insn (operands[0], operands[1]);

  /* need a loop to get all the bits we want  - we generate the
     code at emit time, but need to allocate a scratch reg now  */

  emit_insn (gen_rtx
	     (PARALLEL, VOIDmode,
	      gen_rtvec (2,
			 gen_rtx (SET, VOIDmode, operands[0],
				  gen_rtx (code, mode, operands[0], operands[2])),
			 gen_rtx (CLOBBER, VOIDmode, gen_rtx (SCRATCH, QImode, 0)))));

  return 1;
}

/* Shift algorithm determination.

   There are various ways of doing a shift:
   SHIFT_INLINE: If the amount is small enough, just generate as many one-bit
                 shifts as we need.
   SHIFT_ROT_AND: If the amount is large but close to either end, rotate the
                  necessary bits into position and then set the rest to zero.
   SHIFT_SPECIAL: Hand crafted assembler.
   SHIFT_LOOP:    If the above methods fail, just loop.  */

enum shift_alg
{
  SHIFT_INLINE,
  SHIFT_ROT_AND,
  SHIFT_SPECIAL,
  SHIFT_LOOP,
  SHIFT_MAX
};

/* Symbols of the various shifts which can be used as indices.  */

enum shift_type
  {
    SHIFT_ASHIFT, SHIFT_LSHIFTRT, SHIFT_ASHIFTRT
  };

/* Symbols of the various modes which can be used as indices.  */

enum shift_mode
  {
    QIshift, HIshift, SIshift
  };

/* For single bit shift insns, record assembler and whether the condition code
   is valid afterwards.  */

struct shift_insn
{
  char *assembler;
  int cc_valid;
};

/* Assembler instruction shift table.

   These tables are used to look up the basic shifts.
   They are indexed by cpu, shift_type, and mode.
*/

static const struct shift_insn shift_one[2][3][3] =
{
/* H8/300 */
  {
/* SHIFT_ASHIFT */
    {
      { "shal %X0", 1 },
      { "add.w %T0,%T0\t; shal.w", 1 },
      { "add.w %f0,%f0\t; shal.l\n\taddx %y0,%y0\n\taddx %z0,%z0\t; end shal.l", 0 }
    },
/* SHIFT_LSHIFTRT */
    {
      { "shlr %X0", 1 },
      { "shlr %t0\t; shlr.w\n\trotxr %s0\t; end shlr.w", 0 },
      { "shlr %z0\t; shlr.l\n\trotxr %y0\n\trotxr %x0\n\trotxr %w0\t; end shlr.l", 0 }
    },
/* SHIFT_ASHIFTRT */
    {
      { "shar %X0", 1 },
      { "shar %t0\t; shar.w\n\trotxr %s0\t; end shar.w", 0 },
      { "shar %z0\t; shar.l\n\trotxr %y0\n\trotxr %x0\n\trotxr %w0\t; end shar.l", 0 }
    }
  },
/* H8/300H */
  {
/* SHIFT_ASHIFT */
    {
      { "shal.b %X0", 1 },
      { "shal.w %T0", 1 },
      { "shal.l %S0", 1 }
    },
/* SHIFT_LSHIFTRT */
    {
      { "shlr.b %X0", 1 },
      { "shlr.w %T0", 1 },
      { "shlr.l %S0", 1 }
    },
/* SHIFT_ASHIFTRT */
    {
      { "shar.b %X0", 1 },
      { "shar.w %T0", 1 },
      { "shar.l %S0", 1 }
    }
  }
};

/* Rotates are organized by which shift they'll be used in implementing.
   There's no need to record whether the cc is valid afterwards because
   it is the AND insn that will decide this.  */

static const char *const rotate_one[2][3][3] =
{
/* H8/300 */
  {
/* SHIFT_ASHIFT */
    {
      "rotr %X0",
      "shlr %t0\t; rotr.w\n\trotxr %s0\n\tbst #7,%t0\t; end rotr.w",
      0
    },
/* SHIFT_LSHIFTRT */
    {
      "rotl %X0",
      "shll %s0\t; rotl.w\n\trotxl %t0\n\tbst #0,%s0\t; end rotl.w",
      0
    },
/* SHIFT_ASHIFTRT */
    {
      "rotl %X0",
      "shll %s0\t; rotl.w\n\trotxl %t0\n\tbst #0,%s0\t; end rotl.w",
      0
    }
  },
/* H8/300H */
  {
/* SHIFT_ASHIFT */
    {
      "rotr.b %X0",
      "rotr.w %T0",
      "rotr.l %S0"
    },
/* SHIFT_LSHIFTRT */
    {
      "rotl.b %X0",
      "rotl.w %T0",
      "rotl.l %S0"
    },
/* SHIFT_ASHIFTRT */
    {
      "rotl.b %X0",
      "rotl.w %T0",
      "rotl.l %S0"
    }
  }
};

/* Given CPU, MODE, SHIFT_TYPE, and shift count COUNT, determine the best
   algorithm for doing the shift.  The assembler code is stored in ASSEMBLER.
   We don't achieve maximum efficiency in all cases, but the hooks are here
   to do so.

   For now we just use lots of switch statements.  Since we don't even come
   close to supporting all the cases, this is simplest.  If this function ever
   gets too big, perhaps resort to a more table based lookup.  Of course,
   at this point you may just wish to do it all in rtl.

   WARNING: The constraints on insns shiftbyn_QI/HI/SI assume shifts of
   1,2,3,4 will be inlined (1,2 for SI).  */

static enum shift_alg
get_shift_alg (cpu, shift_type, mode, count, assembler_p, cc_valid_p)
     enum attr_cpu cpu;
     enum shift_type shift_type;
     enum machine_mode mode;
     int count;
     const char **assembler_p;
     int *cc_valid_p;
{
  /* The default is to loop.  */
  enum shift_alg alg = SHIFT_LOOP;
  enum shift_mode shift_mode;

  /* We don't handle negative shifts or shifts greater than the word size,
     they should have been handled already.  */

  if (count < 0 || count > GET_MODE_BITSIZE (mode))
    abort ();

  switch (mode)
    {
    case QImode:
      shift_mode = QIshift;
      break;
    case HImode:
      shift_mode = HIshift;
      break;
    case SImode:
      shift_mode = SIshift;
      break;
    default:
      abort ();
    }

  /* Assume either SHIFT_LOOP or SHIFT_INLINE.
     It is up to the caller to know that looping clobbers cc.  */
  *assembler_p = shift_one[cpu][shift_type][shift_mode].assembler;
  *cc_valid_p = shift_one[cpu][shift_type][shift_mode].cc_valid;

  /* Now look for cases we want to optimize.  */

  switch (shift_mode)
    {
    case QIshift:
      if (count <= 4)
	return SHIFT_INLINE;
      else if (count <= 6)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      return SHIFT_LOOP;
	    }
	  else
	    {
	      *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	      *cc_valid_p = 0;
	      return SHIFT_ROT_AND;
	    }
	}
      else if (count == 7)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      *assembler_p = "shll %X0\t; shar.b(7)\n\tsubx %X0,%X0\t; end shar.b(7)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	  else
	    {
	      *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	      *cc_valid_p = 0;
	      return SHIFT_ROT_AND;
	    }
	}
      break;
    case HIshift:
      if (count <= 4)
	return SHIFT_INLINE;
      else if (count == 8)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.b %s0,%t0\t; shal.w(8)\n\tsub.b %s0,%s0\t; end shal.w(8)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.b %t0,%s0\t; shlr.w(8)\n\tsub.b %t0,%t0\t; end shlr.w(8)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (cpu == CPU_H8300)
		*assembler_p = "mov.b %t0,%s0\t; shar.w(8)\n\tshll %t0\n\tsubx %t0,%t0\t; end shar.w(8)";
	      else
		*assembler_p = "mov.b %t0,%s0\t; shar.w(8)\n\texts.w %T0\t; end shar.w(8)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	  abort ();
	}
      else if (count == 15)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      *assembler_p = "shll %t0,%t0\t; shar.w(15)\n\tsubx %t0,%t0\n\tmov.b %t0,%s0\t; end shar.w(15)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	  else
	    {
	      *assembler_p = rotate_one[cpu][shift_type][shift_mode];
	      *cc_valid_p = 0;
	      return SHIFT_ROT_AND;
	    }
	}
      break;
    case SIshift:
      if (count <= (cpu == CPU_H8300 ? 2 : 4))
	return SHIFT_INLINE;
      else if (count == 8)
	{
	  if (cpu == CPU_H8300)
	    {
	      switch (shift_type)
		{
		case SHIFT_ASHIFT:
		  *assembler_p = "mov.b %y0,%z0\t; shal.l(8)\n\tmov.b %x0,%y0\n\tmov.b %w0,%x0\n\tsub.b %w0,%w0\t; end shal.l(8)";
		  *cc_valid_p = 0;
		  return SHIFT_SPECIAL;
		case SHIFT_LSHIFTRT:
		  *assembler_p = "mov.b %x0,%w0\t; shlr.l(8)\n\tmov.b %y0,%x0\n\tmov.b %z0,%y0\n\tsub.b %z0,%z0\t; end shlr.l(8)";
		  *cc_valid_p = 0;
		  return SHIFT_SPECIAL;
		case SHIFT_ASHIFTRT:
		  *assembler_p = "mov.b %x0,%w0\t; shar.l(8)\n\tmov.b %y0,%x0\n\tmov.b %z0,%y0\n\tshll %z0\n\tsubx %z0,%z0; end shar.l(8)";
		  *cc_valid_p = 0;
		  return SHIFT_SPECIAL;
		}
	    }
	  else			/* CPU_H8300H */
	    /* We don't have byte level access to the high word so this isn't
	       easy to do.  For now, just loop.  */
	    ;
	}
      else if (count == 16)
	{
	  switch (shift_type)
	    {
	    case SHIFT_ASHIFT:
	      *assembler_p = "mov.w %f0,%e0\t; shal.l(16)\n\tsub.w %f0,%f0\t; end shal.l(16)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_LSHIFTRT:
	      *assembler_p = "mov.w %e0,%f0\t; shlr.l(16)\n\tsub.w %e0,%e0\t; end shlr.l(16)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    case SHIFT_ASHIFTRT:
	      if (cpu == CPU_H8300)
		*assembler_p = "mov.w %e0,%f0\t; shar.l(16)\n\tshll %z0\n\tsubx %z0,%z0\n\tmov.b %z0,%y0\t; end shar.l(16)";
	      else
		*assembler_p = "mov.w %e0,%f0\t; shar.l(16)\n\texts.l %S0\t; end shar.l(16)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	}
      else if (count >= 28 && count <= 30)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      return SHIFT_LOOP;
	    }
	  else
	    {
	      if (cpu == CPU_H8300)
		return SHIFT_LOOP;
	      else
		{
		  *assembler_p = rotate_one[cpu][shift_type][shift_mode];
		  *cc_valid_p = 0;
		  return SHIFT_ROT_AND;
		}
	    }
	}
      else if (count == 31)
	{
	  if (shift_type == SHIFT_ASHIFTRT)
	    {
	      if (cpu == CPU_H8300)
		*assembler_p = "shll %z0\t; shar.l(31)\n\tsubx %w0,%w0\n\tmov.b %w0,%x0\n\tmov.w %f0,%e0\t; end shar.l(31)";
	      else
		*assembler_p = "shll %e0\t; shar.l(31)\n\tsubx %w0,%w0\n\tmov.b %w0,%x0\n\tmov.w %f0,%e0\t; end shar.l(31)";
	      *cc_valid_p = 0;
	      return SHIFT_SPECIAL;
	    }
	  else
	    {
	      if (cpu == CPU_H8300)
		{
		  if (shift_type == SHIFT_ASHIFT)
		    *assembler_p = "sub.w %e0,%e0\t; shal.l(31)\n\tshlr %w0\n\tmov.w %e0,%f0\n\trotxr %z0\t; end shal.l(31)";
		  else
		    *assembler_p = "sub.w %f0,%f0\t; shlr.l(31)\n\tshll %z0\n\tmov.w %f0,%e0\n\trotxl %w0\t; end shlr.l(31)";
		  *cc_valid_p = 0;
		  return SHIFT_SPECIAL;
		}
	      else
		{
		  *assembler_p = rotate_one[cpu][shift_type][shift_mode];
		  *cc_valid_p = 0;
		  return SHIFT_ROT_AND;
		}
	    }
	}
      break;
    default:
      abort ();
    }

  return alg;
}

/* Emit the assembler code for doing shifts.  */

char *
emit_a_shift (insn, operands)
     rtx insn;
     rtx *operands;
{
  static int loopend_lab;
  char *assembler;
  int cc_valid;
  rtx inside = PATTERN (insn);
  rtx shift = operands[3];
  enum machine_mode mode = GET_MODE (shift);
  enum rtx_code code = GET_CODE (shift);
  enum shift_type shift_type;
  enum shift_mode shift_mode;

  loopend_lab++;

  switch (mode)
    {
    case QImode:
      shift_mode = QIshift;
      break;
    case HImode:
      shift_mode = HIshift;
      break;
    case SImode:
      shift_mode = SIshift;
      break;
    default:
      abort ();
    }

  switch (code)
    {
    case ASHIFTRT:
      shift_type = SHIFT_ASHIFTRT;
      break;
    case LSHIFTRT:
      shift_type = SHIFT_LSHIFTRT;
      break;
    case ASHIFT:
      shift_type = SHIFT_ASHIFT;
      break;
    default:
      abort ();
    }

  if (GET_CODE (operands[2]) != CONST_INT)
    {
      /* Indexing by reg, so have to loop and test at top */
      output_asm_insn ("mov.b	%X2,%X4", operands);
      fprintf (asm_out_file, "\tble	.Lle%d\n", loopend_lab);

      /* Get the assembler code to do one shift.  */
      get_shift_alg (cpu_type, shift_type, mode, 1, &assembler, &cc_valid);
    }
  else
    {
      int n = INTVAL (operands[2]);
      enum shift_alg alg;

      /* If the count is negative, make it 0.  */
      if (n < 0)
	n = 0;
      /* If the count is too big, truncate it.
         ANSI says shifts of GET_MODE_BITSIZE are undefined - we choose to
	 do the intuitive thing.  */
      else if (n > GET_MODE_BITSIZE (mode))
	n = GET_MODE_BITSIZE (mode);

      alg = get_shift_alg (cpu_type, shift_type, mode, n, &assembler, &cc_valid);

      switch (alg)
	{
	case SHIFT_INLINE:
	  while (--n >= 0)
	    output_asm_insn (assembler, operands);
	  if (cc_valid)
	    cc_status.value1 = operands[0];
	  return "";
	case SHIFT_ROT_AND:
	  {
	    int m = GET_MODE_BITSIZE (mode) - n;
	    int mask = (shift_type == SHIFT_ASHIFT
			? ((1 << GET_MODE_BITSIZE (mode) - n) - 1) << n
			: (1 << GET_MODE_BITSIZE (mode) - n) - 1);
	    char insn_buf[200];
	    /* Not all possibilities of rotate are supported.  They shouldn't
	       be generated, but let's watch for 'em.  */
	    if (assembler == 0)
	      abort ();
	    while (--m >= 0)
	      output_asm_insn (assembler, operands);
	    if (TARGET_H8300)
	      {
		switch (mode)
		  {
		  case QImode:
		    sprintf (insn_buf, "and #%d,%%X0\t; end shift %d via rotate+and",
			     mask, n);
		    cc_status.value1 = operands[0];
		    break;
		  case HImode:
		    sprintf (insn_buf, "and #%d,%%s0\n\tand #%d,%%t0\t; end shift %d via rotate+and",
			     mask & 255, mask >> 8, n);
		    break;
		  case SImode:
		    abort ();
		  }
	      }
	    else
	      {
		sprintf (insn_buf, "and.%c #%d,%%%c0",
			 "bwl"[shift_mode], mask,
			 mode == QImode ? 'X' : mode == HImode ? 'T' : 'S');
		cc_status.value1 = operands[0];
	      }
	    output_asm_insn (insn_buf, operands);
	    return "";
	  }
	case SHIFT_SPECIAL:
	  output_asm_insn (assembler, operands);
	  return "";
	}

      /* Need a loop, move limit to tmp reg */
      fprintf (asm_out_file, "\tmov.b	#%d,%sl\n", n, names_big[REGNO (operands[4])]);
    }

  fprintf (asm_out_file, ".Llt%d:\n", loopend_lab);
  output_asm_insn (assembler, operands);
  output_asm_insn ("add	#0xff,%X4", operands);
  fprintf (asm_out_file, "\tbne	.Llt%d\n", loopend_lab);
  fprintf (asm_out_file, ".Lle%d:\n", loopend_lab);

  return "";
}

/* Fix the operands of a gen_xxx so that it could become a bit
  operating insn.  */

int
fix_bit_operand (operands, what, type)
     rtx *operands;
     char what;
     enum rtx_code type;
{
  /* The bit_operand predicate accepts any memory during RTL generation, but
     only 'U' memory afterwards, so if this is a MEM operand, we must force
     it to be valid for 'U' by reloading the address.  */

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), what))
	{
	  /* Ok to have a memory dest.  */
	  if (GET_CODE (operands[0]) == MEM && !EXTRA_CONSTRAINT (operands[0], 'U'))
	    {
	      rtx mem;
	      mem = gen_rtx (MEM, GET_MODE (operands[0]),
			   copy_to_mode_reg (Pmode, XEXP (operands[0], 0)));
	      RTX_UNCHANGING_P (mem) = RTX_UNCHANGING_P (operands[0]);
	      MEM_IN_STRUCT_P (mem) = MEM_IN_STRUCT_P (operands[0]);
	      MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (operands[0]);
	      operands[0] = mem;
	    }

	  if (GET_CODE (operands[1]) == MEM && !EXTRA_CONSTRAINT (operands[1], 'U'))
	    {
	      rtx mem;
	      mem = gen_rtx (MEM, GET_MODE (operands[1]),
			   copy_to_mode_reg (Pmode, XEXP (operands[1], 0)));
	      RTX_UNCHANGING_P (mem) = RTX_UNCHANGING_P (operands[1]);
	      MEM_IN_STRUCT_P (mem) = MEM_IN_STRUCT_P (operands[1]);
	      MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (operands[1]);
	      operands[1] = mem;
	    }
	  return 0;
	}
    }

  /* Dest and src op must be register.  */

  operands[1] = force_reg (QImode, operands[1]);
  {
    rtx res = gen_reg_rtx (QImode);
    emit_insn (gen_rtx (SET, VOIDmode, res, gen_rtx (type, QImode, operands[1], operands[2])));
    emit_insn (gen_rtx (SET, VOIDmode, operands[0], res));
  }
  return 1;
}
