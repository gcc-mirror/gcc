/* Generate code to allocate RTL structures.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.

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

#define NO_GENRTL_H
#include "rtl.h"

struct rtx_definition {char *enumname, *name, *format; };

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) { STRINGIFY(ENUM), NAME, FORMAT },

struct rtx_definition defs[] = 
{  
#include "rtl.def"		/* rtl expressions are documented here */
};

char *formats[NUM_RTX_CODE];

static char *type_from_format		PROTO((int));
static char *accessor_from_format	PROTO((int));
static int special_format		PROTO((char *));
static int special_rtx			PROTO((int));
static void find_formats		PROTO((void));
static void gendecl			PROTO((char *));
static void genmacro			PROTO((int));
static void gendef			PROTO((char *));
static void genlegend			PROTO((void));
static void genheader			PROTO((void));
static void gencode			PROTO((void));

/* Handle fatal errors.  The macro "abort" calls this.  */

#ifdef HAVE_VPRINTF
void
fatal VPROTO((char *s, ...))
{
#ifndef ANSI_PROTOTYPES
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef ANSI_PROTOTYPES
  s = va_arg (ap, char *);
#endif

  fprintf (stderr, "genconfig: ");
  vfprintf (stderr, s, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}
#else /* not HAVE_VPRINTF */

void
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genconfig: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}
#endif /* not HAVE_VPRINTF */


/* Given a format character used in the type of an RTL parameter, return
   a string giving the C datatype to be used when passing that parameter to
   a function.  We include a trailing blank when the last character is
   not '*'.  */

static char *
type_from_format (c)
     int c;
{
  switch (c)
    {
    case 'i':
      return "int ";

    case 'w':
      return "HOST_WIDE_INT ";

    case 's':
      return "char *";

    case 'e':  case 'u':
      return "rtx ";

    case 'E':
      return "rtvec ";

    /* ?!? These should be bitmap and tree respectively, but those types are
       not available in many of the files which include the output of
       gengenrtl. However, these are only used in prototypes, so void *
       is usable.  */
    case 'b':  case 't':
      return "void *";

    default:
      abort ();
    }
}

/* Similar to above routine, but give the name of the macro used to access
   that type field in a piece of RTL.  */

static char *
accessor_from_format (c)
     int c;
{
  switch (c)
    {
    case 'i':
      return "XINT";

    case 'w':
      return "XWINT";

    case 's':
      return "XSTR";

    case 'e':  case 'u':
      return "XEXP";

    case 'E':
      return "XVEC";

    case 'b':
      return "XBITMAP";

    case 't':
      return "XTREE";

    default:
      abort ();
    }
}

/* Return nonzero if we should ignore FMT, an RTL format, when making
   the list of formats we write routines to create.  */

static int
special_format (fmt)
     char *fmt;
{
  return (strchr (fmt, '*') != 0
	  || strchr (fmt, 'V') != 0
	  || strchr (fmt, 'S') != 0
	  || strchr (fmt, 'n') != 0);
}

/* Return nonzero if the RTL code given by index IDX is one that we should not
   generate a gen_RTX_FOO function foo (because that function is present
   elsewhere in the compiler.  */

static int
special_rtx (idx)
     int idx;
{
  return (strcmp (defs[idx].enumname, "CONST_INT") == 0
	  || strcmp (defs[idx].enumname, "REG") == 0
	  || strcmp (defs[idx].enumname, "MEM") == 0);
}

/* Place a list of all format specifiers we use into the array FORMAT. */

static void
find_formats ()
{
  int i;

  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      char **f;

      if (special_format (defs[i].format))
	continue;

      for (f = formats; *f; f++)
	if (! strcmp (*f, defs[i].format))
	  break;

      if (*f == 0)
	*f = defs[i].format;
    }
}

/* Write the declarations for the routine to allocate RTL with FORMAT.  */

static void
gendecl (format)
     char *format;
{
  char *p;
  int i, pos;
  
  printf ("extern rtx gen_rtx_fmt_%s\tPROTO((RTX_CODE, ", format);
  printf ("enum machine_mode mode");

  /* Write each parameter that is needed and start a new line when the line
     would overflow.  */
  for (p = format, i = 0, pos = 75; *p != 0; p++)
    if (*p != '0')
      {
	int ourlen = strlen (type_from_format (*p)) + 6 + (i > 9);

	printf (",");
	if (pos + ourlen > 76)
	  printf ("\n\t\t\t\t      "), pos = 39;

	printf (" %sarg%d", type_from_format (*p), i++);
	pos += ourlen;
      }

  printf ("));\n");
}

/* Generate macros to generate RTL of code IDX using the functions we
   write.  */

static void 
genmacro (idx)
     int idx;
{
  char *p;
  int i;

  /* We write a macro that defines gen_rtx_RTLCODE to be an equivalent to
     gen_rtx_fmt_FORMAT where FORMAT is the RTX_FORMAT of RTLCODE.  */

  printf ("#define gen_rtx_%s%s(MODE",
	   special_rtx (idx) ? "raw_" : "", defs[idx].enumname);

  for (p = defs[idx].format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", ARG%d", i++);

  printf (") \\\n  gen_rtx_fmt_%s (%s, (MODE)",
	  defs[idx].format, defs[idx].enumname);

  for (p = defs[idx].format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", (ARG%d)", i++);

  printf (")\n");
}

/* Generate the code for the function to generate RTL whose
   format is FORMAT.  */

static void
gendef (format)
     char *format;
{
  char *p;
  int i, j;
  
  /* Start by writing the definition of the function name and the types
     of the arguments.  */

  printf ("rtx\ngen_rtx_fmt_%s (code, mode", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", arg%d", i++);

  printf (")\n     RTX_CODE code;\n     enum machine_mode mode;\n");
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf ("     %sarg%d;\n", type_from_format (*p), i++);

  /* Now write out the body of the function itself, which allocates
     the memory and initializes it.  */
  printf ("{\n");
  printf ("  rtx rt = obstack_alloc_rtx (sizeof (struct rtx_def) + %d",
	  strlen (format) - 1);

  printf (" * sizeof (rtunion));\n\n");

  printf ("  PUT_CODE (rt, code);\n");
  printf ("  PUT_MODE (rt, mode);\n");

  for (p = format, i = j = 0; *p ; ++p, ++i)
    if (*p != '0')
      printf ("  %s (rt, %d) = arg%d;\n", accessor_from_format (*p), i, j++);

  printf ("\n  return rt;\n}\n\n");
}

/* Generate the documentation header for files we write.  */

static void
genlegend ()
{
  printf ("/* Generated automaticaly by the program `gengenrtl'\n");
  printf ("   from the RTL description file `rtl.def' */\n\n");
}

/* Generate the text of the header file we make, genrtl.h.  */

static void
genheader ()
{
  int i;
  char **fmt;
  
  for (fmt = formats; *fmt; ++fmt)
    gendecl (*fmt);

  printf ("\n");

  for (i = 0; i < NUM_RTX_CODE; i++)
    if (! special_format (defs[i].format))
      genmacro (i);
}

/* Generate the text of the code file we write, genrtl.c.  */

static void
gencode ()
{
  char **fmt;

  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"obstack.h\"\n");
  printf ("#include \"rtl.h\"\n\n");

  for (fmt = formats; *fmt != 0; fmt++)
    gendef (*fmt);
}

/* This is the main program.  We accept only one argument, "-h", which
   says we are writing the genrtl.h file.  Otherwise we are writing the
   genrtl.c file.  */

int
main (argc, argv)
     int argc;
     char **argv;
{
  find_formats ();
  genlegend ();

  if (argc == 2 && argv[1][0] == '-' && argv[1][1] == 'h')
    genheader ();
  else
    gencode ();

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
