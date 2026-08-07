/* Generate code from machine description to extract operands from insn as rtl.
   Copyright (C) 1987-2026 Free Software Foundation, Inc.

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
   only the first operand of an rtl expression is a vector.  genrecog.cc makes
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
VEC_char_to_string (const vec<char> &v)
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

/* The character that marks an operand number that a pattern does not use.
   It cannot clash with a path step, which is always a digit or a letter.  */
#define MISSING_OPERAND_CHAR '!'

/* The paths of all extraction methods, concatenated.  Each path is NUL
   terminated; the paths of one method are adjacent, operands first and
   dups second.  */
static struct obstack pathpool;

/* The dup numbers of all extraction methods, concatenated.  */
static vec<int> dupnums;

/* Add PATH, which is null for an operand number that the pattern skips,
   to the path pool.  */

static void
add_path (const char *path)
{
  if (path)
    obstack_grow (&pathpool, path, strlen (path));
  else
    obstack_1grow (&pathpool, MISSING_OPERAND_CHAR);
  obstack_1grow (&pathpool, '\0');
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
}

/* Print STR as a C string literal, broken into chunks so that no output
   line gets excessively long.  */

static void
print_string_literal (const char *str, unsigned int len)
{
  printf ("  \"");
  for (unsigned int i = 0; i < len; i++)
    {
      if (str[i] == '\0')
	/* Spell the terminator with all three octal digits: the next
	   character may be a digit, which a shorter escape would absorb.  */
	printf ("\\000");
      else
	putchar (str[i]);
      if ((i % 60) == 59 && i + 1 < len)
	printf ("\"\n  \"");
    }
  printf ("\"");
}

/* Print the tables that drive insn_extract, and insn_extract itself.  */

static void
print_extractions (void)
{
  struct extraction *p;
  struct code_ptr *link;
  unsigned int i;

  /* Number the methods and record, for every insn code, the method that
     extracts its operands.  Method 0 means "not an extractable insn" and
     method 1 means "an old-style define_peephole", whose operand count is
     only known at run time.  */
  auto_vec<unsigned int> method_of_code;
  auto_vec<struct extraction *> methods;
  auto_vec<unsigned int> path_start;
  auto_vec<unsigned int> dup_start;

  for (link = peepholes; link; link = link->next)
    {
      while (method_of_code.length () <= (unsigned int) link->insn_code)
	method_of_code.safe_push (0);
      method_of_code[link->insn_code] = 1;
    }

  obstack_init (&pathpool);
  for (p = extractions; p; p = p->next)
    {
      unsigned int method = methods.length () + 2;
      gcc_assert (method <= USHRT_MAX);
      gcc_assert (p->op_count <= UCHAR_MAX && p->dup_count <= UCHAR_MAX);
      path_start.safe_push (obstack_object_size (&pathpool));
      dup_start.safe_push (dupnums.length ());
      methods.safe_push (p);
      for (i = 0; i < p->op_count; i++)
	add_path (p->oplocs[i]);
      for (i = 0; i < p->dup_count; i++)
	{
	  add_path (p->duplocs[i]);
	  gcc_assert (IN_RANGE (p->dupnums[i], 0, UCHAR_MAX));
	  dupnums.safe_push (p->dupnums[i]);
	}
      for (link = p->insns; link; link = link->next)
	{
	  while (method_of_code.length () <= (unsigned int) link->insn_code)
	    method_of_code.safe_push (0);
	  method_of_code[link->insn_code] = method;
	}
    }
  path_start.safe_push (obstack_object_size (&pathpool));

  unsigned int pool_len = obstack_object_size (&pathpool);
  const char *pool = XOBFINISH (&pathpool, const char *);

  printf ("/* The paths that locate the operands and the dups of each\n"
	  "   extraction method.  A path is a sequence of steps down the\n"
	  "   pattern: a digit D selects XEXP (x, D - '0'), a lower-case\n"
	  "   letter L selects XVECEXP (x, 0, L - 'a') and an upper-case\n"
	  "   letter U selects XVECEXP (x, 0, U - %d).  An empty path denotes\n"
	  "   the pattern itself and '%c' an operand number that the pattern\n"
	  "   does not use.  */\n", UPPER_OFFSET, MISSING_OPERAND_CHAR);
  printf ("#define UPPER_OFFSET %d\n", UPPER_OFFSET);
  printf ("#define MISSING_OPERAND_CHAR '%c'\n\n", MISSING_OPERAND_CHAR);
  printf ("static const char extract_paths[] =\n");
  print_string_literal (pool, pool_len);
  printf (";\n\n");

  printf ("static const unsigned char extract_dup_num[] = {");
  for (i = 0; i < dupnums.length (); i++)
    printf ("%s%d,", (i % 20) == 0 ? "\n  " : " ", dupnums[i]);
  printf ("%s0\n};\n\n", dupnums.length () ? "\n  " : "");

  printf ("struct extract_method_d {\n"
	  "  unsigned int paths;\n"
	  "  unsigned int dups;\n"
	  "  unsigned char n_operands;\n"
	  "  unsigned char n_dups;\n"
	  "};\n\n");

  printf ("static const struct extract_method_d extract_methods[] = {\n"
	  "  { 0, 0, 0, 0 },\n"
	  "  { 0, 0, 0, 0 },\n");
  for (i = 0; i < methods.length (); i++)
    printf ("  { %u, %u, %u, %u },\n", path_start[i], dup_start[i],
	    methods[i]->op_count, methods[i]->dup_count);
  printf ("};\n\n");

  printf ("static const unsigned short extract_method_of_code[] = {");
  for (i = 0; i < method_of_code.length (); i++)
    printf ("%s%u,", (i % 20) == 0 ? "\n  " : " ", method_of_code[i]);
  printf ("\n};\n\n");

  puts ("\
/* Follow one NUL-terminated path in extract_paths from *PP, starting at\n\
   the pattern *ROOT, and return the location it selects.  *PP is left\n\
   just after the path's terminator.  */\n\
\n\
static inline rtx *\n\
follow_extract_path (const char **pp, rtx *root)\n{\n\
  const char *p = *pp;\n\
  rtx *loc = root;\n\
  for (; *p; p++)\n\
    if (ISDIGIT (*p))\n\
      loc = &XEXP (*loc, *p - '0');\n\
    else if (ISLOWER (*p))\n\
      loc = &XVECEXP (*loc, 0, *p - 'a');\n\
    else\n\
      loc = &XVECEXP (*loc, 0, *p - UPPER_OFFSET);\n\
  *pp = p + 1;\n\
  return loc;\n\
}\n");

  puts ("\
void\n\
insn_extract (rtx_insn *insn)\n{\n\
  rtx *ro = recog_data.operand;\n\
  rtx **ro_loc = recog_data.operand_loc;\n\
  int icode = INSN_CODE (insn);\n\
\n\
  if (flag_checking)\n\
    {\n\
      memset (ro, 0xab, sizeof (*ro) * MAX_RECOG_OPERANDS);\n\
      memset (ro_loc, 0xab, sizeof (*ro_loc) * MAX_RECOG_OPERANDS);\n\
    }\n\
\n\
  unsigned int method = (icode >= 0\n\
			 && icode < (int) ARRAY_SIZE (extract_method_of_code)\n\
			 ? extract_method_of_code[icode] : 0);\n\
  if (method == 0)\n\
    {\n\
      /* Control reaches here if insn_extract has been called with an\n\
	 unrecognizable insn (code -1), or an insn whose INSN_CODE\n\
	 corresponds to a DEFINE_EXPAND in the machine description;\n\
	 either way, a bug.  */\n\
      if (icode < 0)\n\
	fatal_insn (\"unrecognizable insn:\", insn);\n\
      else\n\
	fatal_insn (\"insn with invalid code number:\", insn);\n\
    }\n\
\n\
  if (method == 1)\n\
    {\n\
      /* An old-style define_peephole.  The vector in the insn was created\n\
	 just for this function and contains nothing but operands.  */\n\
      for (int i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)\n\
	ro[i] = *(ro_loc[i] = &XVECEXP (PATTERN (insn), 0, i));\n\
      return;\n\
    }\n\
\n\
  const struct extract_method_d *m = &extract_methods[method];\n\
  const char *p = extract_paths + m->paths;\n\
  for (unsigned int i = 0; i < m->n_operands; i++)\n\
    if (*p == MISSING_OPERAND_CHAR)\n\
      {\n\
	ro[i] = const0_rtx;\n\
	ro_loc[i] = &junk;\n\
	p += 2;\n\
      }\n\
    else\n\
      {\n\
	rtx *loc = follow_extract_path (&p, &PATTERN (insn));\n\
	ro_loc[i] = loc;\n\
	ro[i] = *loc;\n\
      }\n\
  for (unsigned int i = 0; i < m->n_dups; i++)\n\
    {\n\
      recog_data.dup_loc[i] = follow_extract_path (&p, &PATTERN (insn));\n\
      recog_data.dup_num[i] = extract_dup_num[m->dups + i];\n\
    }\n}");
}

int
main (int argc, const char **argv)
{
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
  print_extractions ();
  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
