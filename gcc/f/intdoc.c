/* intdoc.c
   Copyright (C) 1997, 2000, 2001 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* From f/proj.h, which uses #error -- not all C compilers
   support that, and we want *this* program to be compilable
   by pretty much any C compiler.  */
#include "hconfig.h"
#include "system.h"
#include "assert.h"

/* Pull in the intrinsics info, but only the doc parts.  */
#define FFEINTRIN_DOC 1
#include "intrin.h"

const char *family_name (ffeintrinFamily family);
static void dumpif (ffeintrinFamily fam);
static void dumpendif (void);
static void dumpclearif (void);
static void dumpem (void);
static void dumpgen (int menu, const char *name, const char *name_uc,
		     ffeintrinGen gen);
static void dumpspec (int menu, const char *name, const char *name_uc,
		      ffeintrinSpec spec);
static void dumpimp (int menu, const char *name, const char *name_uc, size_t genno, ffeintrinFamily family,
		     ffeintrinImp imp, ffeintrinSpec spec);
static const char *argument_info_ptr (ffeintrinImp imp, int argno);
static const char *argument_info_string (ffeintrinImp imp, int argno);
static const char *argument_name_ptr (ffeintrinImp imp, int argno);
static const char *argument_name_string (ffeintrinImp imp, int argno);
#if 0
static const char *elaborate_if_complex (ffeintrinImp imp, int argno);
static const char *elaborate_if_maybe_complex (ffeintrinImp imp, int argno);
static const char *elaborate_if_real (ffeintrinImp imp, int argno);
#endif
static void print_type_string (const char *c);

int
main (int argc, char **argv ATTRIBUTE_UNUSED)
{
  if (argc != 1)
    {
      fprintf (stderr, "\
Usage: intdoc > intdoc.texi\n\
  Collects and dumps documentation on g77 intrinsics\n\
  to the file named intdoc.texi.\n");
      exit (1);
    }

  dumpem ();
  return 0;
}

struct _ffeintrin_name_
  {
    const char *const name_uc;
    const char *const name_lc;
    const char *const name_ic;
    const ffeintrinGen generic;
    const ffeintrinSpec specific;
  };

struct _ffeintrin_gen_
  {
    const char *const name;		/* Name as seen in program. */
    const ffeintrinSpec specs[2];
  };

struct _ffeintrin_spec_
  {
    const char *const name;	/* Uppercase name as seen in source code,
				   lowercase if no source name, "none" if no
				   name at all (NONE case). */
    const bool is_actualarg;	/* Ok to pass as actual arg if -pedantic. */
    const ffeintrinFamily family;
    const ffeintrinImp implementation;
  };

struct _ffeintrin_imp_
  {
    const char *const name;		/* Name of implementation. */
    const char *const control;
  };

static const struct _ffeintrin_name_ names[] = {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC) \
  { UPPER, LOWER, MIXED, FFEINTRIN_ ## GEN, FFEINTRIN_ ## SPEC },
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

static const struct _ffeintrin_gen_ gens[] = {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2) \
  { NAME, { SPEC1, SPEC2, }, },
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

static const struct _ffeintrin_imp_ imps[] = {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL) \
  { NAME, CONTROL },
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD) \
  { NAME, CONTROL },
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

static const struct _ffeintrin_spec_ specs[] = {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP) \
  { NAME, CALLABLE, FAMILY, IMP, },
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

struct cc_pair { const ffeintrinImp imp; const char *const text; };

static const char *descriptions[FFEINTRIN_imp] = { 0 };
static const struct cc_pair cc_descriptions[] = {
#define DEFDOC(IMP,SUMMARY,DESCRIPTION) { FFEINTRIN_imp ## IMP, DESCRIPTION },
#include "intdoc.h0"
#undef DEFDOC
};

static const char *summaries[FFEINTRIN_imp] = { 0 };
static const struct cc_pair cc_summaries[] = {
#define DEFDOC(IMP,SUMMARY,DESCRIPTION) { FFEINTRIN_imp ## IMP, SUMMARY },
#include "intdoc.h0"
#undef DEFDOC
};

const char *
family_name (ffeintrinFamily family)
{
  switch (family)
    {
    case FFEINTRIN_familyF77:
      return "familyF77";

    case FFEINTRIN_familyASC:
      return "familyASC";

    case FFEINTRIN_familyMIL:
      return "familyMIL";

    case FFEINTRIN_familyGNU:
      return "familyGNU";

    case FFEINTRIN_familyF90:
      return "familyF90";

    case FFEINTRIN_familyVXT:
      return "familyVXT";

    case FFEINTRIN_familyFVZ:
      return "familyFVZ";

    case FFEINTRIN_familyF2C:
      return "familyF2C";

    case FFEINTRIN_familyF2U:
      return "familyF2U";

    case FFEINTRIN_familyBADU77:
      return "familyBADU77";

    default:
      assert ("bad family" == NULL);
      return "??";
    }
}

static int in_ifset = 0;
static ffeintrinFamily latest_family = FFEINTRIN_familyNONE;

static void
dumpif (ffeintrinFamily fam)
{
  assert (fam != FFEINTRIN_familyNONE);
  if ((in_ifset != 2)
      || (fam != latest_family))
    {
      if (in_ifset == 2)
	printf ("@end ifset\n");
      latest_family = fam;
      printf ("@ifset %s\n", family_name (fam));
    }
  in_ifset = 1;
}

static void
dumpendif ()
{
  in_ifset = 2;
}

static void
dumpclearif ()
{
  if ((in_ifset == 2)
      || (latest_family != FFEINTRIN_familyNONE))
    printf ("@end ifset\n");
  latest_family = FFEINTRIN_familyNONE;
  in_ifset = 0;
}

static void
dumpem ()
{
  int i;

  for (i = 0; ((size_t) i) < ARRAY_SIZE (cc_descriptions); ++i)
    {
      assert (descriptions[cc_descriptions[i].imp] == NULL);
      descriptions[cc_descriptions[i].imp] = cc_descriptions[i].text;
    }

  for (i = 0; ((size_t) i) < ARRAY_SIZE (cc_summaries); ++i)
    {
      assert (summaries[cc_summaries[i].imp] == NULL);
      summaries[cc_summaries[i].imp] = cc_summaries[i].text;
    }

  printf ("@c This file is automatically derived from intdoc.c, intdoc.in,\n");
  printf ("@c ansify.c, intrin.def, and intrin.h.  Edit those files instead.\n");
  printf ("@menu\n");
  for (i = 0; ((size_t) i) < ARRAY_SIZE (names); ++i)
    {
      if (names[i].generic != FFEINTRIN_genNONE)
	dumpgen (1, names[i].name_ic, names[i].name_uc,
		 names[i].generic);
      if (names[i].specific != FFEINTRIN_specNONE)
	dumpspec (1, names[i].name_ic, names[i].name_uc,
		  names[i].specific);
    }
  dumpclearif ();

  printf ("@end menu\n\n");

  for (i = 0; ((size_t) i) < ARRAY_SIZE (names); ++i)
    {
      if (names[i].generic != FFEINTRIN_genNONE)
	dumpgen (0, names[i].name_ic, names[i].name_uc,
		 names[i].generic);
      if (names[i].specific != FFEINTRIN_specNONE)
	dumpspec (0, names[i].name_ic, names[i].name_uc,
		  names[i].specific);
    }
  dumpclearif ();
}

static void
dumpgen (int menu, const char *name, const char *name_uc, ffeintrinGen gen)
{
  size_t i;
  int total = 0;

  if (!menu)
    {
      for (i = 0; i < ARRAY_SIZE (gens[gen].specs); ++i)
	{
	  if (gens[gen].specs[i] != FFEINTRIN_specNONE)
	    ++total;
	}
    }

  for (i = 0; i < ARRAY_SIZE (gens[gen].specs); ++i)
    {
      ffeintrinSpec spec;
      size_t j;

      if ((spec = gens[gen].specs[i]) == FFEINTRIN_specNONE)
	continue;

      dumpif (specs[spec].family);
      dumpimp (menu, name, name_uc, i, specs[spec].family, specs[spec].implementation,
	       spec);
      if (!menu && (total > 0))
	{
	  if (total == 1)
	    {
	      printf ("\
For information on another intrinsic with the same name:\n");
	    }
	  else
	    {
	      printf ("\
For information on other intrinsics with the same name:\n");
	    }
	  for (j = 0; j < ARRAY_SIZE (gens[gen].specs); ++j)
	    {
	      if (j == i)
		continue;
	      if ((spec = gens[gen].specs[j]) == FFEINTRIN_specNONE)
		continue;
	      printf ("@xref{%s Intrinsic (%s)}.\n",
		      name, specs[spec].name);
	    }
	  printf ("\n");
	}
      dumpendif ();
    }
}

static void
dumpspec (int menu, const char *name, const char *name_uc, ffeintrinSpec spec)
{
  dumpif (specs[spec].family);
  dumpimp (menu, name, name_uc, 0, specs[spec].family, specs[spec].implementation,
	   FFEINTRIN_specNONE);
  dumpendif ();
}

static void
dumpimp (int menu, const char *name, const char *name_uc, size_t genno,
	 ffeintrinFamily family, ffeintrinImp imp, ffeintrinSpec spec)
{
  const char *c;
  bool subr;
  const char *argc;
  const char *argi;
  int colon;
  int argno;

  assert ((imp != FFEINTRIN_impNONE) || !genno);

  if (menu)
    {
      printf ("* %s Intrinsic",
	      name);
      if (spec != FFEINTRIN_specNONE)
	printf (" (%s)", specs[spec].name);	/* See XYZZY1 below */
      printf ("::");
#define INDENT_SUMMARY 24
      if ((imp == FFEINTRIN_impNONE)
	  || (summaries[imp] != NULL))
	{
	  int spaces = INDENT_SUMMARY - 14 - strlen (name);
	  const char *c;

	  if (spec != FFEINTRIN_specNONE)
	    spaces -= (3 + strlen (specs[spec].name));	/* See XYZZY1 above */
	  if (spaces < 1)
	    spaces = 1;
	  while (spaces--)
	    fputc (' ', stdout);

	  if (imp == FFEINTRIN_impNONE)
	    {
	      printf ("(Reserved for future use.)\n");
	      return;
	    }

	  for (c = summaries[imp]; c[0] != '\0'; ++c)
	    {
	      if (c[0] == '@' && ISDIGIT (c[1]))
		{
		  int argno = c[1] - '0';

		  c += 2;
		  while (ISDIGIT (c[0]))
		    {
		      argno = 10 * argno + (c[0] - '0');
		      ++c;
		    }
		  assert (c[0] == '@');
		  if (argno == 0)
		    printf ("%s", name);
		  else if (argno == 99)
		    {	/* Yeah, this is a major kludge. */
		      printf ("\n");
		      spaces = INDENT_SUMMARY + 1;
		      while (spaces--)
			fputc (' ', stdout);
		    }
		  else
		    printf ("%s", argument_name_string (imp, argno - 1));
		}
	      else
		fputc (c[0], stdout);
	    }
	}
      printf ("\n");
      return;
    }

  printf ("@node %s Intrinsic", name);
  if (spec != FFEINTRIN_specNONE)
    printf (" (%s)", specs[spec].name);
  printf ("\n@subsubsection %s Intrinsic", name);
  if (spec != FFEINTRIN_specNONE)
    printf (" (%s)", specs[spec].name);
  printf ("\n@cindex %s intrinsic\n@cindex intrinsics, %s\n",
	  name, name);

  if (imp == FFEINTRIN_impNONE)
    {
      printf ("\n\
This intrinsic is not yet implemented.\n\
The name is, however, reserved as an intrinsic.\n\
Use @samp{EXTERNAL %s} to use this name for an\n\
external procedure.\n\
\n\
",
	      name);
      return;
    }

  c = imps[imp].control;
  subr = (c[0] == '-');
  colon = (c[2] == ':') ? 2 : 3;

  printf ("\n\
@noindent\n\
@example\n\
%s%s(",
	  (subr ? "CALL " : ""), name);

  fflush (stdout);

  for (argno = 0; ; ++argno)
    {
      argc = argument_name_ptr (imp, argno);
      if (argc == NULL)
	break;
      if (argno > 0)
	printf (", ");
      printf ("@var{%s}", argc);
      argi = argument_info_string (imp, argno);
      if ((argi[0] == '*')
	  || (argi[0] == 'n')
	  || (argi[0] == '+')
	  || (argi[0] == 'p'))
	printf ("-1, @var{%s}-2, @dots{}, @var{%s}-n",
		argc, argc);
    }

  printf (")\n\
@end example\n\
\n\
");

  if (!subr)
    {
      int other_arg;
      const char *arg_string;
      const char *arg_info;

      if (ISDIGIT (c[colon + 1]))
	{
	  other_arg = c[colon + 1] - '0';
	  arg_string = argument_name_string (imp, other_arg);
	  arg_info = argument_info_string (imp, other_arg);
	}
      else
	{
	  other_arg = -1;
	  arg_string = NULL;
	  arg_info = NULL;
	}

      printf ("\
@noindent\n\
%s: ", name);
      print_type_string (c);
      printf (" function");

      if ((c[0] == 'R')
	  && (c[1] == 'C'))
	{
	  assert (other_arg >= 0);

	  if ((arg_info[0] == '?') || (arg_info[0] == '!') || (arg_info[0] == '+')
	  || (arg_info[0] == '*') || (arg_info[0] == 'n') || (arg_info[0] == 'p'))
	    ++arg_info;
	  if ((arg_info[0] == 'F') || (arg_info[0] == 'N'))
	    printf (".\n\
The exact type is @samp{REAL(KIND=1)} when argument @var{%s} is\n\
any type other than @code{COMPLEX}, or when it is @code{COMPLEX(KIND=1)}.\n\
When @var{%s} is any @code{COMPLEX} type other than @code{COMPLEX(KIND=1)},\n\
this intrinsic is valid only when used as the argument to\n\
@code{REAL()}, as explained below.\n\n",
		    arg_string,
		    arg_string);
	  else
	    printf (".\n\
This intrinsic is valid when argument @var{%s} is\n\
@code{COMPLEX(KIND=1)}.\n\
When @var{%s} is any other @code{COMPLEX} type,\n\
this intrinsic is valid only when used as the argument to\n\
@code{REAL()}, as explained below.\n\n",
		    arg_string,
		    arg_string);
	}
#if 0
      else if ((c[0] == 'I')
	       && (c[1] == '7'))
	printf (", the exact type being wide enough to hold a pointer\n\
on the target system (typically @code{INTEGER(KIND=1)} or @code{INTEGER(KIND=4)}).\n\n");
#endif
      else if (c[1] == '=' && ISDIGIT (c[colon + 1]))
	{
	  assert (other_arg >= 0);

	  if ((arg_info[0] == '?') || (arg_info[0] == '!') || (arg_info[0] == '+')
	  || (arg_info[0] == '*') || (arg_info[0] == 'n') || (arg_info[0] == 'p'))
	    ++arg_info;

	  if (((c[0] == arg_info[0])
	       && ((c[0] == 'A') || (c[0] == 'C') || (c[0] == 'I')
		   || (c[0] == 'L') || (c[0] == 'R')))
	      || ((c[0] == 'R')
		  && (arg_info[0] == 'C'))
	      || ((c[0] == 'C')
		  && (arg_info[0] == 'R')))
	    printf (", the @samp{KIND=} value of the type being that of argument @var{%s}.\n\n",
		    arg_string);
	  else if ((c[0] == 'S')
		   && ((arg_info[0] == 'C')
		       || (arg_info[0] == 'F')
		       || (arg_info[0] == 'N')))
	    printf (".\n\
The exact type depends on that of argument @var{%s}---if @var{%s} is\n\
@code{COMPLEX}, this function's type is @code{REAL}\n\
with the same @samp{KIND=} value as the type of @var{%s}.\n\
Otherwise, this function's type is the same as that of @var{%s}.\n\n",
		    arg_string, arg_string, arg_string, arg_string);
	  else
	    printf (", the exact type being that of argument @var{%s}.\n\n",
		    arg_string);
	}
      else if ((c[1] == '=')
	       && (c[colon + 1] == '*'))
	printf (", the exact type being the result of cross-promoting the\n\
types of all the arguments.\n\n");
      else if (c[1] == '=')
	assert ("?0:?:" == NULL);
      else
	printf (".\n\n");
    }

  for (argno = 0, argc = &c[colon + 3]; *argc != '\0'; ++argno)
    {
      char optionality = '\0';
      char extra = '\0';
      char basic;
      char kind;
      int length;
      int elements;

      printf ("\
@noindent\n\
@var{");
      for (; ; ++argc)
	{
	  if (argc[0] == '=')
	    break;
	  printf ("%c", *argc);
	}
      printf ("}: ");

      ++argc;
      if ((*argc == '?')
	  || (*argc == '!')
	  || (*argc == '*')
	  || (*argc == '+')
	  || (*argc == 'n')
	  || (*argc == 'p'))
	optionality = *(argc++);
      basic = *(argc++);
      kind = *(argc++);
      if (*argc == '[')
	{
	  length = *++argc - '0';
	  if (*++argc != ']')
	    length = 10 * length + (*(argc++) - '0');
	  ++argc;
	}
      else
	length = -1;
      if (*argc == '(')
	{
	  elements = *++argc - '0';
	  if (*++argc != ')')
	    elements = 10 * elements + (*(argc++) - '0');
	  ++argc;
	}
      else if (*argc == '&')
	{
	  elements = -1;
	  ++argc;
	}
      else
	elements = 0;
      if ((*argc == '&')
	  || (*argc == 'i')
	  || (*argc == 'w')
	  || (*argc == 'x'))
	extra = *(argc++);
      if (*argc == ',')
	++argc;

      switch (basic)
	{
	case '-':
	  switch (kind)
	    {
	    case '*':
	      printf ("Any type");
	      break;

	    default:
	      assert ("kind arg" == NULL);
	      break;
	    }
	  break;

	case 'A':
	  assert ((kind == '1') || (kind == '*'));
	  printf ("@code{CHARACTER");
	  if (length != -1)
	    printf ("*%d", length);
	  printf ("}");
	  break;

	case 'C':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{COMPLEX}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{COMPLEX(KIND=%d)}", (kind - '0'));
	      break;

	    case 'A':
	      printf ("Same @samp{KIND=} value as for @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    default:
	      assert ("Ca" == NULL);
	      break;
	    }
	  break;

	case 'I':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{INTEGER}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{INTEGER(KIND=%d)}", (kind - '0'));
	      break;

	    case 'A':
	      printf ("@code{INTEGER} with same @samp{KIND=} value as for @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    case 'N':
	      printf ("@code{INTEGER} not wider than the default kind");
	      break;

	    default:
	      assert ("Ia" == NULL);
	      break;
	    }
	  break;

	case 'L':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{LOGICAL}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{LOGICAL(KIND=%d)}", (kind - '0'));
	      break;

	    case 'A':
	      printf ("@code{LOGICAL} with same @samp{KIND=} value as for @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    case 'N':
	      printf ("@code{LOGICAL} not wider than the default kind");
	      break;

	    default:
	      assert ("La" == NULL);
	      break;
	    }
	  break;

	case 'R':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{REAL}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{REAL(KIND=%d)}", (kind - '0'));
	      break;

	    case 'A':
	      printf ("@code{REAL} with same @samp{KIND=} value as for @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    default:
	      assert ("Ra" == NULL);
	      break;
	    }
	  break;

	case 'B':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{INTEGER} or @code{LOGICAL}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{INTEGER(KIND=%d)} or @code{LOGICAL(KIND=%d)}",
		      (kind - '0'), (kind - '0'));
	      break;

	    case 'A':
	      printf ("Same type and @samp{KIND=} value as for @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    case 'N':
	      printf ("@code{INTEGER} or @code{LOGICAL} not wider than the default kind");
	      break;

	    default:
	      assert ("Ba" == NULL);
	      break;
	    }
	  break;

	case 'F':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{REAL} or @code{COMPLEX}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{REAL(KIND=%d)} or @code{COMPLEX(KIND=%d)}",
		      (kind - '0'), (kind - '0'));
	      break;

	    case 'A':
	      printf ("Same type as @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    default:
	      assert ("Fa" == NULL);
	      break;
	    }
	  break;

	case 'N':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{INTEGER}, @code{REAL}, or @code{COMPLEX}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{INTEGER(KIND=%d)}, @code{REAL(KIND=%d)}, or @code{COMPLEX(KIND=%d)}",
		      (kind - '0'), (kind - '0'), (kind - '0'));
	      break;

	    default:
	      assert ("N1" == NULL);
	      break;
	    }
	  break;

	case 'S':
	  switch (kind)
	    {
	    case '*':
	      printf ("@code{INTEGER} or @code{REAL}");
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      printf ("@code{INTEGER(KIND=%d)} or @code{REAL(KIND=%d)}",
		      (kind - '0'), (kind - '0'));
	      break;

	    case 'A':
	      printf ("@code{INTEGER} or @code{REAL} with same @samp{KIND=} value as for @var{%s}",
		      argument_name_string (imp, 0));
	      break;

	    default:
	      assert ("Sa" == NULL);
	      break;
	    }
	  break;

	case 'g':
	  printf ("@samp{*@var{label}}, where @var{label} is the label\n\
of an executable statement");
	  break;

	case 's':
	  printf ("Signal handler (@code{INTEGER FUNCTION} or @code{SUBROUTINE})\n\
or dummy/global @code{INTEGER(KIND=1)} scalar");
	  break;

	default:
	  assert ("arg type?" == NULL);
	  break;
	}

      switch (optionality)
	{
	case '\0':
	  break;

	case '!':
	  printf ("; OPTIONAL (must be omitted if @var{%s} is @code{COMPLEX})",
		  argument_name_string (imp, argno-1));
	  break;

	case '?':
	  printf ("; OPTIONAL");
	  break;

	case '*':
	  printf ("; OPTIONAL");
	  break;

	case 'n':
	case '+':
	  break;

	case 'p':
	  printf ("; at least two such arguments must be provided");
	  break;

	default:
	  assert ("optionality!" == NULL);
	  break;
	}

      switch (elements)
	{
	case -1:
	  break;

	case 0:
	  if ((basic != 'g')
	      && (basic != 's'))
	    printf ("; scalar");
	  break;

	default:
	  assert (extra != '\0');
	  printf ("; DIMENSION(%d)", elements);
	  break;
	}

      switch (extra)
	{
	case '\0':
	  if ((basic != 'g')
	      && (basic != 's'))
	    printf ("; INTENT(IN)");
	  break;

	case 'i':
	  break;

	case '&':
	  printf ("; cannot be a constant or expression");
	  break;

	case 'w':
	  printf ("; INTENT(OUT)");
	  break;

	case 'x':
	  printf ("; INTENT(INOUT)");
	  break;
	}

      printf (".\n\n");
    }

  printf ("\
@noindent\n\
Intrinsic groups: ");
  switch (family)
    {
    case FFEINTRIN_familyF77:
      printf ("(standard FORTRAN 77).");
      break;

    case FFEINTRIN_familyGNU:
      printf ("@code{gnu}.");
      break;

    case FFEINTRIN_familyASC:
      printf ("@code{f2c}, @code{f90}.");
      break;

    case FFEINTRIN_familyMIL:
      printf ("@code{mil}, @code{f90}, @code{vxt}.");
      break;

    case FFEINTRIN_familyF90:
      printf ("@code{f90}.");
      break;

    case FFEINTRIN_familyVXT:
      printf ("@code{vxt}.");
      break;

    case FFEINTRIN_familyFVZ:
      printf ("@code{f2c}, @code{vxt}.");
      break;

    case FFEINTRIN_familyF2C:
      printf ("@code{f2c}.");
      break;

    case FFEINTRIN_familyF2U:
      printf ("@code{unix}.");
      break;

    case FFEINTRIN_familyBADU77:
      printf ("@code{badu77}.");
      break;

    default:
      assert ("bad family" == NULL);
      printf ("@code{???}.");
      break;
    }
  printf ("\n\n");

  if (descriptions[imp] != NULL)
    {
      const char *c = descriptions[imp];

      printf ("\
@noindent\n\
Description:\n\
\n");

      while (c[0] != '\0')
	{
	  if (c[0] == '@' && ISDIGIT (c[1]))
	    {
	      int argno = c[1] - '0';

	      c += 2;
	      while (ISDIGIT (c[0]))
		{
		  argno = 10 * argno + (c[0] - '0');
		  ++c;
		}
	      assert (c[0] == '@');
	      if (argno == 0)
		printf ("%s", name_uc);
	      else
		printf ("%s", argument_name_string (imp, argno - 1));
	    }
	  else
	    fputc (c[0], stdout);
	  ++c;
	}

      printf ("\n");
    }
}

static const char *
argument_info_ptr (ffeintrinImp imp, int argno)
{
  const char *c = imps[imp].control;
  static char arginfos[8][32];
  static int argx = 0;
  int i;

  if (c[2] == ':')
    c += 5;
  else
    c += 6;

  while (argno--)
    {
      while ((c[0] != ',') && (c[0] != '\0'))
	++c;
      if (c[0] != ',')
	break;
      ++c;
    }

  if (c[0] == '\0')
    return NULL;

  for (; (c[0] != '=') && (c[0] != '\0'); ++c)
    ;

  assert (c[0] == '=');

  for (i = 0, ++c; (c[0] != ',') && (c[0] != '\0'); ++c, ++i)
    arginfos[argx][i] = c[0];

  arginfos[argx][i] = '\0';

  c = &arginfos[argx][0];
  ++argx;
  if (((size_t) argx) >= ARRAY_SIZE (arginfos))
    argx = 0;

  return c;
}

static const char *
argument_info_string (ffeintrinImp imp, int argno)
{
  const char *p;

  p = argument_info_ptr (imp, argno);
  assert (p != NULL);
  return p;
}

static const char *
argument_name_ptr (ffeintrinImp imp, int argno)
{
  const char *c = imps[imp].control;
  static char argnames[8][32];
  static int argx = 0;
  int i;

  if (c[2] == ':')
    c += 5;
  else
    c += 6;

  while (argno--)
    {
      while ((c[0] != ',') && (c[0] != '\0'))
	++c;
      if (c[0] != ',')
	break;
      ++c;
    }

  if (c[0] == '\0')
    return NULL;

  for (i = 0; (c[0] != '=') && (c[0] != '\0'); ++c, ++i)
    argnames[argx][i] = c[0];

  assert (c[0] == '=');
  argnames[argx][i] = '\0';

  c = &argnames[argx][0];
  ++argx;
  if (((size_t) argx) >= ARRAY_SIZE (argnames))
    argx = 0;

  return c;
}

static const char *
argument_name_string (ffeintrinImp imp, int argno)
{
  const char *p;

  p = argument_name_ptr (imp, argno);
  assert (p != NULL);
  return p;
}

static void
print_type_string (const char *c)
{
  char basic = c[0];
  char kind = c[1];

  switch (basic)
    {
    case 'A':
      assert ((kind == '1') || (kind == '='));
      if (c[2] == ':')
	printf ("@code{CHARACTER*1}");
      else
	{
	  assert (c[2] == '*');
	  printf ("@code{CHARACTER*(*)}");
	}
      break;

    case 'C':
      switch (kind)
	{
	case '=':
	  printf ("@code{COMPLEX}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{COMPLEX(KIND=%d)}", (kind - '0'));
	  break;

	default:
	  assert ("Ca" == NULL);
	  break;
	}
      break;

    case 'I':
      switch (kind)
	{
	case '=':
	  printf ("@code{INTEGER}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{INTEGER(KIND=%d)}", (kind - '0'));
	  break;

	default:
	  assert ("Ia" == NULL);
	  break;
	}
      break;

    case 'L':
      switch (kind)
	{
	case '=':
	  printf ("@code{LOGICAL}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{LOGICAL(KIND=%d)}", (kind - '0'));
	  break;

	default:
	  assert ("La" == NULL);
	  break;
	}
      break;

    case 'R':
      switch (kind)
	{
	case '=':
	  printf ("@code{REAL}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{REAL(KIND=%d)}", (kind - '0'));
	  break;

	case 'C':
	  printf ("@code{REAL}");
	  break;

	default:
	  assert ("Ra" == NULL);
	  break;
	}
      break;

    case 'B':
      switch (kind)
	{
	case '=':
	  printf ("@code{INTEGER} or @code{LOGICAL}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{INTEGER(KIND=%d)} or @code{LOGICAL(KIND=%d)}",
		  (kind - '0'), (kind - '0'));
	  break;

	default:
	  assert ("Ba" == NULL);
	  break;
	}
      break;

    case 'F':
      switch (kind)
	{
	case '=':
	  printf ("@code{REAL} or @code{COMPLEX}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{REAL(KIND=%d)} or @code{COMPLEX(KIND=%d)}",
		  (kind - '0'), (kind - '0'));
	  break;

	default:
	  assert ("Fa" == NULL);
	  break;
	}
      break;

    case 'N':
      switch (kind)
	{
	case '=':
	  printf ("@code{INTEGER}, @code{REAL}, or @code{COMPLEX}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{INTEGER(KIND=%d)}, @code{REAL(KIND=%d)}, or @code{COMPLEX(KIND=%d)}",
		  (kind - '0'), (kind - '0'), (kind - '0'));
	  break;

	default:
	  assert ("N1" == NULL);
	  break;
	}
      break;

    case 'S':
      switch (kind)
	{
	case '=':
	  printf ("@code{INTEGER} or @code{REAL}");
	  break;

	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  printf ("@code{INTEGER(KIND=%d)} or @code{REAL(KIND=%d)}",
		  (kind - '0'), (kind - '0'));
	  break;

	default:
	  assert ("Sa" == NULL);
	  break;
	}
      break;

    default:
      assert ("type?" == NULL);
      break;
    }
}
