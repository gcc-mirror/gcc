/* Generate code from machine description to extract operands from insn as rtl.
   Copyright (C) 1987, 1991, 1992, 1993, 1997, 1998,
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
#include "insn-config.h"
#include "gensupport.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* This structure contains all the information needed to describe one
   set of extractions methods.  Each method may be used by more than 
   one pattern if the operands are in the same place.

   The string for each operand describes that path to the operand and
   contains `0' through `9' when going into an expression and `a' through
   `z' when going into a vector.  We assume here that only the first operand
   of an rtl expression is a vector.  genrecog.c makes the same assumption
   (and uses the same representation) and it is currently true.  */

struct extraction
{
  int op_count;
  char *oplocs[MAX_RECOG_OPERANDS];
  int dup_count;
  char *duplocs[MAX_DUP_OPERANDS];
  int dupnums[MAX_DUP_OPERANDS];
  struct code_ptr *insns;
  struct extraction *next;
};

/* Holds a single insn code that use an extraction method.  */

struct code_ptr
{
  int insn_code;
  struct code_ptr *next;
};

static struct extraction *extractions;

/* Holds an array of names indexed by insn_code_number.  */
static char **insn_name_ptr = 0;
static int insn_name_ptr_size = 0;

/* Number instruction patterns handled, starting at 0 for first one.  */

static int insn_code_number;

/* Records the large operand number in this insn.  */

static int op_count;

/* Records the location of any operands using the string format described
   above.  */

static char *oplocs[MAX_RECOG_OPERANDS];

/* Number the occurrences of MATCH_DUP in each instruction,
   starting at 0 for the first occurrence.  */

static int dup_count;

/* Records the location of any MATCH_DUP operands.  */

static char *duplocs[MAX_DUP_OPERANDS];

/* Record the operand number of any MATCH_DUPs.  */

static int dupnums[MAX_DUP_OPERANDS];

/* Record the list of insn_codes for peepholes.  */

static struct code_ptr *peepholes;

static void gen_insn PARAMS ((rtx));
static void walk_rtx PARAMS ((rtx, const char *));
static void print_path PARAMS ((const char *));
static void record_insn_name PARAMS ((int, const char *));

static void
gen_insn (insn)
     rtx insn;
{
  register int i;
  register struct extraction *p;
  register struct code_ptr *link;

  op_count = 0;
  dup_count = 0;

  /* No operands seen so far in this pattern.  */
  memset (oplocs, 0, sizeof oplocs);

  /* Walk the insn's pattern, remembering at all times the path
     down to the walking point.  */

  if (XVECLEN (insn, 1) == 1)
    walk_rtx (XVECEXP (insn, 1, 0), "");
  else
    for (i = XVECLEN (insn, 1) - 1; i >= 0; i--)
      {
	char *path = (char *) alloca (2);

	path[0] = 'a' + i;
	path[1] = 0;

	walk_rtx (XVECEXP (insn, 1, i), path);
      }

  link = (struct code_ptr *) xmalloc (sizeof (struct code_ptr));
  link->insn_code = insn_code_number;

  /* See if we find something that already had this extraction method.  */

  for (p = extractions; p; p = p->next)
    {
      if (p->op_count != op_count || p->dup_count != dup_count)
	continue;

      for (i = 0; i < op_count; i++)
	if (p->oplocs[i] != oplocs[i]
	    && ! (p->oplocs[i] != 0 && oplocs[i] != 0
		  && ! strcmp (p->oplocs[i], oplocs[i])))
	  break;

      if (i != op_count)
	continue;

      for (i = 0; i < dup_count; i++)
	if (p->dupnums[i] != dupnums[i]
	    || strcmp (p->duplocs[i], duplocs[i]))
	  break;

      if (i != dup_count)
	continue;

      /* This extraction is the same as ours.  Just link us in.  */
      link->next = p->insns;
      p->insns = link;
      return;
    }

  /* Otherwise, make a new extraction method.  */

  p = (struct extraction *) xmalloc (sizeof (struct extraction));
  p->op_count = op_count;
  p->dup_count = dup_count;
  p->next = extractions;
  extractions = p;
  p->insns = link;
  link->next = 0;

  for (i = 0; i < op_count; i++)
    p->oplocs[i] = oplocs[i];

  for (i = 0; i < dup_count; i++)
    p->dupnums[i] = dupnums[i], p->duplocs[i] = duplocs[i];
}

static void
walk_rtx (x, path)
     rtx x;
     const char *path;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register const char *fmt;
  int depth = strlen (path);
  char *newpath;

  if (x == 0)
    return;

  code = GET_CODE (x);

  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case SYMBOL_REF:
      return;

    case MATCH_OPERAND:
    case MATCH_SCRATCH:
      oplocs[XINT (x, 0)] = xstrdup (path);
      op_count = MAX (op_count, XINT (x, 0) + 1);
      break;

    case MATCH_DUP:
    case MATCH_PAR_DUP:
      duplocs[dup_count] = xstrdup (path);
      dupnums[dup_count] = XINT (x, 0);
      dup_count++;
      break;

    case MATCH_OP_DUP:
      duplocs[dup_count] = xstrdup (path);
      dupnums[dup_count] = XINT (x, 0);
      dup_count++;
      
      newpath = (char *) alloca (depth + 2);
      strcpy (newpath, path);
      newpath[depth + 1] = 0;
      
      for (i = XVECLEN (x, 1) - 1; i >= 0; i--)
        {
	  newpath[depth] = '0' + i;
	  walk_rtx (XVECEXP (x, 1, i), newpath);
        }
      return;
      
    case MATCH_OPERATOR:
      oplocs[XINT (x, 0)] = xstrdup (path);
      op_count = MAX (op_count, XINT (x, 0) + 1);

      newpath = (char *) alloca (depth + 2);
      strcpy (newpath, path);
      newpath[depth + 1] = 0;

      for (i = XVECLEN (x, 2) - 1; i >= 0; i--)
	{
	  newpath[depth] = '0' + i;
	  walk_rtx (XVECEXP (x, 2, i), newpath);
	}
      return;

    case MATCH_PARALLEL:
      oplocs[XINT (x, 0)] = xstrdup (path);
      op_count = MAX (op_count, XINT (x, 0) + 1);

      newpath = (char *) alloca (depth + 2);
      strcpy (newpath, path);
      newpath[depth + 1] = 0;

      for (i = XVECLEN (x, 2) - 1; i >= 0; i--)
	{
	  newpath[depth] = 'a' + i;
	  walk_rtx (XVECEXP (x, 2, i), newpath);
	}
      return;

    case ADDRESS:
      walk_rtx (XEXP (x, 0), path);
      return;

    default:
      break;
    }

  newpath = (char *) alloca (depth + 2);
  strcpy (newpath, path);
  newpath[depth + 1] = 0;

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	{
	  newpath[depth] = '0' + i;
	  walk_rtx (XEXP (x, i), newpath);
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      newpath[depth] = 'a' + j;
	      walk_rtx (XVECEXP (x, i, j), newpath);
	    }
	}
    }
}

/* Given a PATH, representing a path down the instruction's
   pattern from the root to a certain point, output code to
   evaluate to the rtx at that point.  */

static void
print_path (path)
     const char *path;
{
  register int len = strlen (path);
  register int i;

  if (len == 0)
    {
      /* Don't emit "pat", since we may try to take the address of it,
	 which isn't what is intended.  */
      printf("PATTERN (insn)");
      return;
    }

  /* We first write out the operations (XEXP or XVECEXP) in reverse
     order, then write "insn", then the indices in forward order.  */

  for (i = len - 1; i >=0 ; i--)
    {
      if (ISLOWER(path[i]))
	printf ("XVECEXP (");
      else if (ISDIGIT(path[i]))
	printf ("XEXP (");
      else
	abort ();
    }
  
  printf ("pat");

  for (i = 0; i < len; i++)
    {
      if (ISLOWER(path[i]))
	printf (", 0, %d)", path[i] - 'a');
      else if (ISDIGIT(path[i]))
	printf (", %d)", path[i] - '0');
      else
	abort ();
    }
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

char *
xstrdup (input)
  const char *input;
{
  register size_t len = strlen (input) + 1;
  register char *output = xmalloc (len);
  memcpy (output, input, len);
  return output;
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  int i;
  struct extraction *p;
  struct code_ptr *link;
  const char *name;

  progname = "genextract";
  obstack_init (rtl_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  if (init_md_reader (argv[1]) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  /* Assign sequential codes to all entries in the machine description
     in parallel with the tables in insn-output.c.  */

  insn_code_number = 0;

  printf ("/* Generated automatically by the program `genextract'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"recog.h\"\n");
  printf ("#include \"toplev.h\"\n\n");

  /* This variable exists only so it can be the "location"
     of any missing operand whose numbers are skipped by a given pattern.  */
  printf ("static rtx junk ATTRIBUTE_UNUSED;\n");

  printf ("void\ninsn_extract (insn)\n");
  printf ("     rtx insn;\n");
  printf ("{\n");
  printf ("  register rtx *ro = recog_data.operand;\n");
  printf ("  register rtx **ro_loc = recog_data.operand_loc;\n");
  printf ("  rtx pat = PATTERN (insn);\n");
  printf ("  int i ATTRIBUTE_UNUSED;\n\n");
  printf ("  memset (ro, 0, sizeof (*ro) * MAX_RECOG_OPERANDS);\n");
  printf ("  memset (ro_loc, 0, sizeof (*ro_loc) * MAX_RECOG_OPERANDS);\n");
  printf ("  switch (INSN_CODE (insn))\n");
  printf ("    {\n");
  printf ("    case -1:\n");
  printf ("      fatal_insn_not_found (insn);\n\n");

  /* Read the machine description.  */

  while (1)
    {
      int line_no;

      desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;

       if (GET_CODE (desc) == DEFINE_INSN)
	{
	  record_insn_name (insn_code_number, XSTR (desc, 0));
	  gen_insn (desc);
	}

      else if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	{
	  struct code_ptr *link
	    = (struct code_ptr *) xmalloc (sizeof (struct code_ptr));

	  link->insn_code = insn_code_number;
	  link->next = peepholes;
	  peepholes = link;
	}
    }

  /* Write out code to handle peepholes and the insn_codes that it should
     be called for.  */
  if (peepholes)
    {
      for (link = peepholes; link; link = link->next)
	printf ("    case %d:\n", link->insn_code);

      /* The vector in the insn says how many operands it has.
	 And all it contains are operands.  In fact, the vector was
	 created just for the sake of this function.  */
      printf ("      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)\n");
      printf ("          ro[i] = XVECEXP (pat, 0, i);\n");
      printf ("      break;\n\n");
    }

  /* Write out all the ways to extract insn operands.  */
  for (p = extractions; p; p = p->next)
    {
      for (link = p->insns; link; link = link->next)
	{
	  i = link->insn_code;
	  name = get_insn_name (i);
	  if (name)
	    printf ("    case %d:  /* %s */\n", i, name);
	  else
	    printf ("    case %d:\n", i);
	}
      
      for (i = 0; i < p->op_count; i++)
	{
	  if (p->oplocs[i] == 0)
	    {
	      printf ("      ro[%d] = const0_rtx;\n", i);
	      printf ("      ro_loc[%d] = &junk;\n", i);
	    }
	  else
	    {
	      printf ("      ro[%d] = *(ro_loc[%d] = &", i, i);
	      print_path (p->oplocs[i]);
	      printf (");\n");
	    }
	}

      for (i = 0; i < p->dup_count; i++)
	{
	  printf ("      recog_data.dup_loc[%d] = &", i);
	  print_path (p->duplocs[i]);
	  printf (";\n");
	  printf ("      recog_data.dup_num[%d] = %d;\n", i, p->dupnums[i]);
	}

      printf ("      break;\n\n");
    }

  /* This should never be reached.  Note that we would also reach this abort
   if we tried to extract something whose INSN_CODE was a DEFINE_EXPAND or
   DEFINE_SPLIT, but that is correct.  */
  printf ("    default:\n      abort ();\n");

  printf ("    }\n}\n");

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */
const char *
get_insn_name (code)
     int code ATTRIBUTE_UNUSED;
{
  if (code < insn_name_ptr_size)
    return insn_name_ptr[code];
  else
    return NULL;
}

static void
record_insn_name (code, name)
     int code;
     const char *name;
{
  static const char *last_real_name = "insn";
  static int last_real_code = 0;
  char *new;

  if (insn_name_ptr_size <= code)
    {
      int new_size;
      new_size = (insn_name_ptr_size ? insn_name_ptr_size * 2 : 512);
      insn_name_ptr =
	(char **) xrealloc (insn_name_ptr, sizeof(char *) * new_size);
      memset (insn_name_ptr + insn_name_ptr_size, 0, 
	      sizeof(char *) * (new_size - insn_name_ptr_size));
      insn_name_ptr_size = new_size;
    }

  if (!name || name[0] == '\0')
    {
      new = xmalloc (strlen (last_real_name) + 10);
      sprintf (new, "%s+%d", last_real_name, code - last_real_code);
    }
  else
    {
      last_real_name = new = xstrdup (name);
      last_real_code = code;
    }
  
  insn_name_ptr[code] = new;
}  
