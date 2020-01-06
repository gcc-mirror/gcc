/* Generate code from machine description to extract operands from insn as rtl.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"

/* This structure contains all the information needed to describe one
   set of extractions methods.  Each method may be used by more than
   one pattern if the operands are in the same place.

   The string for each operand describes that path to the operand and
   contains `0' through `9' when going into an expression and `a' through
   `z' then 'A' through to 'Z' when going into a vector.  We assume here that
   only the first operand of an rtl expression is a vector.  genrecog.c makes
   the same assumption (and uses the same representation) and it is currently
   true.  */

typedef char *locstr;

struct extraction
{
  unsigned int op_count;
  unsigned int dup_count;
  locstr *oplocs;
  locstr *duplocs;
  int *dupnums;
  struct code_ptr *insns;
  struct extraction *next;
};

/* Holds a single insn code that uses an extraction method.  */
struct code_ptr
{
  int insn_code;
  struct code_ptr *next;
};

/* All extractions needed for this machine description.  */
static struct extraction *extractions;

/* All insn codes for old-style peepholes.  */
static struct code_ptr *peepholes;

/* This structure is used by gen_insn and walk_rtx to accumulate the
   data that will be used to produce an extractions structure.  */


class accum_extract
{
public:
  accum_extract () : oplocs (10), duplocs (10), dupnums (10), pathstr (20) {}

  auto_vec<locstr> oplocs;
  auto_vec<locstr> duplocs;
  auto_vec<int> dupnums;
  auto_vec<char> pathstr;
};

/* Forward declarations.  */
static void walk_rtx (md_rtx_info *, rtx, class accum_extract *);

#define UPPER_OFFSET ('A' - ('z' - 'a' + 1))

/* Convert integer OPERAND into a character - either into [a-zA-Z] for vector
   operands or [0-9] for integer operands - and push onto the end of the path
   in ACC.  */
static void
push_pathstr_operand (int operand, bool is_vector,
		     class accum_extract *acc)
{
  if (is_vector && 'a' + operand > 'z')
    acc->pathstr.safe_push (operand + UPPER_OFFSET);
  else if (is_vector)
    acc->pathstr.safe_push (operand + 'a');
  else
    acc->pathstr.safe_push (operand + '0');
}

static void
gen_insn (md_rtx_info *info)
{
  int i;
  unsigned int op_count, dup_count, j;
  struct extraction *p;
  struct code_ptr *link;
  class accum_extract acc;

  /* Walk the insn's pattern, remembering at all times the path
     down to the walking point.  */

  rtx insn = info->def;
  if (XVECLEN (insn, 1) == 1)
    walk_rtx (info, XVECEXP (insn, 1, 0), &acc);
  else
    for (i = XVECLEN (insn, 1) - 1; i >= 0; i--)
      {
	push_pathstr_operand (i, true, &acc);
	walk_rtx (info, XVECEXP (insn, 1, i), &acc);
	acc.pathstr.pop ();
      }

  link = XNEW (struct code_ptr);
  link->insn_code = info->index;

  /* See if we find something that already had this extraction method.  */

  op_count = acc.oplocs.length ();
  dup_count = acc.duplocs.length ();
  gcc_assert (dup_count == acc.dupnums.length ());

  for (p = extractions; p; p = p->next)
    {
      if (p->op_count != op_count || p->dup_count != dup_count)
	continue;

      for (j = 0; j < op_count; j++)
	{
	  char *a = p->oplocs[j];
	  char *b = acc.oplocs[j];
	  if (a != b && (!a || !b || strcmp (a, b)))
	    break;
	}

      if (j != op_count)
	continue;

      for (j = 0; j < dup_count; j++)
	if (p->dupnums[j] != acc.dupnums[j]
	    || strcmp (p->duplocs[j], acc.duplocs[j]))
	  break;

      if (j != dup_count)
	continue;

      /* This extraction is the same as ours.  Just link us in.  */
      link->next = p->insns;
      p->insns = link;
      return;
    }

  /* Otherwise, make a new extraction method.  We stash the arrays
     after the extraction structure in memory.  */

  p = XNEWVAR (struct extraction, sizeof (struct extraction)
	       + op_count*sizeof (char *)
	       + dup_count*sizeof (char *)
	       + dup_count*sizeof (int));
  p->op_count = op_count;
  p->dup_count = dup_count;
  p->next = extractions;
  extractions = p;
  p->insns = link;
  link->next = 0;

  p->oplocs = (char **)((char *)p + sizeof (struct extraction));
  p->duplocs = p->oplocs + op_count;
  p->dupnums = (int *)(p->duplocs + dup_count);

  memcpy (p->oplocs, acc.oplocs.address (), op_count * sizeof (locstr));
  memcpy (p->duplocs, acc.duplocs.address (), dup_count * sizeof (locstr));
  memcpy (p->dupnums, acc.dupnums.address (), dup_count * sizeof (int));
}

/* Helper subroutine of walk_rtx: given a vec<locstr>, an index, and a
   string, insert the string at the index, which should either already
   exist and be NULL, or not yet exist within the vector.  In the latter
   case the vector is enlarged as appropriate.  INFO describes the
   containing define_* expression.  */
static void
VEC_safe_set_locstr (md_rtx_info *info, vec<locstr> *vp,
		     unsigned int ix, char *str)
{
  if (ix < (*vp).length ())
    {
      if ((*vp)[ix])
	{
	  message_at (info->loc, "repeated operand number %d", ix);
	  have_error = 1;
	}
      else
        (*vp)[ix] = str;
    }
  else
    {
      while (ix > (*vp).length ())
	vp->safe_push (NULL);
      vp->safe_push (str);
    }
}

/* Another helper subroutine of walk_rtx: given a vec<char>, convert it
   to a NUL-terminated string in malloc memory.  */
static char *
VEC_char_to_string (vec<char> v)
{
  size_t n = v.length ();
  char *s = XNEWVEC (char, n + 1);
  memcpy (s, v.address (), n);
  s[n] = '\0';
  return s;
}

static void
walk_rtx (md_rtx_info *info, rtx x, class accum_extract *acc)
{
  RTX_CODE code;
  int i, len;
  const char *fmt;

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
      VEC_safe_set_locstr (info, &acc->oplocs, XINT (x, 0),
			   VEC_char_to_string (acc->pathstr));
      break;

    case MATCH_OPERATOR:
    case MATCH_PARALLEL:
      VEC_safe_set_locstr (info, &acc->oplocs, XINT (x, 0),
			   VEC_char_to_string (acc->pathstr));

      for (i = XVECLEN (x, 2) - 1; i >= 0; i--)
	{
	  push_pathstr_operand (i, code != MATCH_OPERATOR, acc);
	  walk_rtx (info, XVECEXP (x, 2, i), acc);
	  acc->pathstr.pop ();
        }
      return;

    case MATCH_DUP:
    case MATCH_PAR_DUP:
    case MATCH_OP_DUP:
      acc->duplocs.safe_push (VEC_char_to_string (acc->pathstr));
      acc->dupnums.safe_push (XINT (x, 0));

      if (code == MATCH_DUP)
	break;

      for (i = XVECLEN (x, 1) - 1; i >= 0; i--)
        {
	  push_pathstr_operand (i, code != MATCH_OP_DUP, acc);
	  walk_rtx (info, XVECEXP (x, 1, i), acc);
	  acc->pathstr.pop ();
        }
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	{
	  push_pathstr_operand (i, false, acc);
	  walk_rtx (info, XEXP (x, i), acc);
	  acc->pathstr.pop ();
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      push_pathstr_operand (j, true, acc);
	      walk_rtx (info, XVECEXP (x, i, j), acc);
	      acc->pathstr.pop ();
	    }
	}
    }
}

/* Given a PATH, representing a path down the instruction's
   pattern from the root to a certain point, output code to
   evaluate to the rtx at that point.  */

static void
print_path (const char *path)
{
  int len = strlen (path);
  int i;

  if (len == 0)
    {
      /* Don't emit "pat", since we may try to take the address of it,
	 which isn't what is intended.  */
      fputs ("PATTERN (insn)", stdout);
      return;
    }

  /* We first write out the operations (XEXP or XVECEXP) in reverse
     order, then write "pat", then the indices in forward order.  */

  for (i = len - 1; i >= 0 ; i--)
    {
      if (ISLOWER (path[i]) || ISUPPER (path[i]))
	fputs ("XVECEXP (", stdout);
      else if (ISDIGIT (path[i]))
	fputs ("XEXP (", stdout);
      else
	gcc_unreachable ();
    }

  fputs ("pat", stdout);

  for (i = 0; i < len; i++)
    {
      if (ISUPPER (path[i]))
	printf (", 0, %d)", path[i] - UPPER_OFFSET);
      else if (ISLOWER (path[i]))
	printf (", 0, %d)", path[i] - 'a');
      else if (ISDIGIT (path[i]))
	printf (", %d)", path[i] - '0');
      else
	gcc_unreachable ();
    }
}

static void
print_header (void)
{
  /* N.B. Code below avoids putting squiggle braces in column 1 inside
     a string, because this confuses some editors' syntax highlighting
     engines.  */

  puts ("\
/* Generated automatically by the program `genextract'\n\
   from the machine description file `md'.  */\n\
\n\
#define IN_TARGET_CODE 1\n\
#include \"config.h\"\n\
#include \"system.h\"\n\
#include \"coretypes.h\"\n\
#include \"tm.h\"\n\
#include \"rtl.h\"\n\
#include \"insn-config.h\"\n\
#include \"recog.h\"\n\
#include \"diagnostic-core.h\"\n\
\n\
/* This variable is used as the \"location\" of any missing operand\n\
   whose numbers are skipped by a given pattern.  */\n\
static rtx junk ATTRIBUTE_UNUSED;\n");

  puts ("\
void\n\
insn_extract (rtx_insn *insn)\n{\n\
  rtx *ro = recog_data.operand;\n\
  rtx **ro_loc = recog_data.operand_loc;\n\
  rtx pat = PATTERN (insn);\n\
  int i ATTRIBUTE_UNUSED; /* only for peepholes */\n\
\n\
  if (flag_checking)\n\
    {\n\
      memset (ro, 0xab, sizeof (*ro) * MAX_RECOG_OPERANDS);\n\
      memset (ro_loc, 0xab, sizeof (*ro_loc) * MAX_RECOG_OPERANDS);\n\
    }\n");

  puts ("\
  switch (INSN_CODE (insn))\n\
    {\n\
    default:\n\
      /* Control reaches here if insn_extract has been called with an\n\
         unrecognizable insn (code -1), or an insn whose INSN_CODE\n\
         corresponds to a DEFINE_EXPAND in the machine description;\n\
         either way, a bug.  */\n\
      if (INSN_CODE (insn) < 0)\n\
        fatal_insn (\"unrecognizable insn:\", insn);\n\
      else\n\
        fatal_insn (\"insn with invalid code number:\", insn);\n");
}

int
main (int argc, const char **argv)
{
  unsigned int i;
  struct extraction *p;
  struct code_ptr *link;
  const char *name;

  progname = "genextract";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
	gen_insn (&info);
	break;

      case DEFINE_PEEPHOLE:
	{
	  struct code_ptr *link = XNEW (struct code_ptr);

	  link->insn_code = info.index;
	  link->next = peepholes;
	  peepholes = link;
	}
	break;

      default:
	break;
    }

  if (have_error)
    return FATAL_EXIT_CODE;

  print_header ();

  /* Write out code to handle peepholes and the insn_codes that it should
     be called for.  */
  if (peepholes)
    {
      for (link = peepholes; link; link = link->next)
	printf ("    case %d:\n", link->insn_code);

      /* The vector in the insn says how many operands it has.
	 And all it contains are operands.  In fact, the vector was
	 created just for the sake of this function.  We need to set the
	 location of the operands for sake of simplifications after
	 extraction, like eliminating subregs.  */
      puts ("      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)\n"
	    "          ro[i] = *(ro_loc[i] = &XVECEXP (pat, 0, i));\n"
	    "      break;\n");
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
	      puts (");");
	    }
	}

      for (i = 0; i < p->dup_count; i++)
	{
	  printf ("      recog_data.dup_loc[%d] = &", i);
	  print_path (p->duplocs[i]);
	  puts (";");
	  printf ("      recog_data.dup_num[%d] = %d;\n", i, p->dupnums[i]);
	}

      puts ("      break;\n");
    }

  puts ("    }\n}");
  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
