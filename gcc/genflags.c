/* Generate from machine description:
   - some flags HAVE_... saying which simple standard instructions are
   available for this machine.
   Copyright (C) 1987, 1991, 1995, 1998,
   1999, 2000 Free Software Foundation, Inc.

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


#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"
#include "gensupport.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Obstacks to remember normal, and call insns.  */
static struct obstack call_obstack, normal_obstack;

/* Max size of names encountered.  */
static int max_id_len;

/* Max operand encountered in a scan over some insn.  */
static int max_opno;

static void max_operand_1	PARAMS ((rtx));
static int num_operands		PARAMS ((rtx));
static void gen_proto		PARAMS ((rtx));
static void gen_nonproto	PARAMS ((rtx));
static void gen_insn		PARAMS ((rtx));

/* Count the number of match_operand's found.  */

static void
max_operand_1 (x)
     rtx x;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register const char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  if (code == MATCH_OPERAND || code == MATCH_OPERATOR
      || code == MATCH_PARALLEL)
    max_opno = MAX (max_opno, XINT (x, 0));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	max_operand_1 (XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    max_operand_1 (XVECEXP (x, i, j));
	}
    }
}

static int
num_operands (insn)
     rtx insn;
{
  register int len = XVECLEN (insn, 1);
  register int i;

  max_opno = -1;

  for (i = 0; i < len; i++)
    max_operand_1 (XVECEXP (insn, 1, i));

  return max_opno + 1;
}

/* Print out prototype information for a function.  */

static void
gen_proto (insn)
     rtx insn;
{
  int num = num_operands (insn);
  printf ("extern rtx gen_%-*s PARAMS ((", max_id_len, XSTR (insn, 0));

  if (num == 0)
    printf ("void");
  else
    {
      while (num-- > 1)
	printf ("rtx, ");

      printf ("rtx");
    }

  printf ("));\n");
}

/* Print out a function declaration without a prototype.  */

static void
gen_nonproto (insn)
     rtx insn;
{
  printf ("extern rtx gen_%s ();\n", XSTR (insn, 0));
}

static void
gen_insn (insn)
     rtx insn;
{
  const char *name = XSTR (insn, 0);
  const char *p;
  struct obstack *obstack_ptr;
  int len;

  /* Don't mention instructions whose names are the null string
     or begin with '*'.  They are in the machine description just
     to be recognized.  */
  if (name[0] == 0 || name[0] == '*')
    return;

  len = strlen (name);

  if (len > max_id_len)
    max_id_len = len;

  printf ("#define HAVE_%s ", name);
  if (strlen (XSTR (insn, 2)) == 0)
    printf ("1\n");
  else
    {
      /* Write the macro definition, putting \'s at the end of each line,
	 if more than one.  */
      printf ("(");
      for (p = XSTR (insn, 2); *p; p++)
	{
	  if (*p == '\n')
	    printf (" \\\n");
	  else
	    printf ("%c", *p);
	}
      printf (")\n");
    }

  /* Save the current insn, so that we can later put out appropriate
     prototypes.  At present, most md files have the wrong number of
     arguments for the call insns (call, call_value, call_pop,
     call_value_pop) ignoring the extra arguments that are passed for
     some machines, so by default, turn off the prototype.  */

  obstack_ptr = ((name[0] == 'c' || name[0] == 's')
		 && (!strcmp (name, "call")
		     || !strcmp (name, "call_value")
		     || !strcmp (name, "call_pop")
		     || !strcmp (name, "call_value_pop")
		     || !strcmp (name, "sibcall")
		     || !strcmp (name, "sibcall_value")
		     || !strcmp (name, "sibcall_pop")
		     || !strcmp (name, "sibcall_value_pop")))
    ? &call_obstack : &normal_obstack;

  obstack_grow (obstack_ptr, &insn, sizeof (rtx));
}

PTR
xmalloc (size)
  size_t size;
{
  register PTR val = (PTR) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");

  return val;
}

PTR
xrealloc (old, size)
  PTR old;
  size_t size;
{
  register PTR ptr;
  if (old)
    ptr = (PTR) realloc (old, size);
  else
    ptr = (PTR) malloc (size);
  if (!ptr)
    fatal ("virtual memory exhausted");
  return ptr;
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  rtx dummy;
  rtx *call_insns;
  rtx *normal_insns;
  rtx *insn_ptr;

  progname = "genflags";
  obstack_init (rtl_obstack);
  obstack_init (&call_obstack);
  obstack_init (&normal_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  if (init_md_reader (argv[1]) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);
  
  printf ("/* Generated automatically by the program `genflags'\n\
from the machine description file `md'.  */\n\n");

  /* Read the machine description.  */

  while (1)
    {
      int line_no, insn_code_number = 0;

      desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;
      if (GET_CODE (desc) == DEFINE_INSN || GET_CODE (desc) == DEFINE_EXPAND)
	gen_insn (desc);
    }

  /* Print out the prototypes now.  */
  dummy = (rtx) 0;
  obstack_grow (&call_obstack, &dummy, sizeof (rtx));
  call_insns = (rtx *) obstack_finish (&call_obstack);

  obstack_grow (&normal_obstack, &dummy, sizeof (rtx));
  normal_insns = (rtx *) obstack_finish (&normal_obstack);

  for (insn_ptr = normal_insns; *insn_ptr; insn_ptr++)
    gen_proto (*insn_ptr);

  printf ("\n#ifdef MD_CALL_PROTOTYPES\n");
  for (insn_ptr = call_insns; *insn_ptr; insn_ptr++)
    gen_proto (*insn_ptr);

  printf ("\n#else /* !MD_CALL_PROTOTYPES */\n");
  for (insn_ptr = call_insns; *insn_ptr; insn_ptr++)
    gen_nonproto (*insn_ptr);

  printf ("#endif /* !MD_CALL_PROTOTYPES */\n");

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */
const char *
get_insn_name (code)
     int code ATTRIBUTE_UNUSED;
{
  return NULL;
}
