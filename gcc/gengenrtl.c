/* Generate code to allocate RTL structures.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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
#undef abort

#include "real.h"

/* Calculate the format for CONST_DOUBLE.  This depends on the relative
   widths of HOST_WIDE_INT and REAL_VALUE_TYPE.

   We need to go out to e0wwwww, since REAL_ARITHMETIC assumes 16-bits
   per element in REAL_VALUE_TYPE.

   This is duplicated in rtl.c.

   A number of places assume that there are always at least two 'w'
   slots in a CONST_DOUBLE, so we provide them even if one would suffice.  */

#ifdef REAL_ARITHMETIC
#if LONG_DOUBLE_TYPE_SIZE == 96
#define REAL_WIDTH	(11*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#elif LONG_DOUBLE_TYPE_SIZE == 128
#define REAL_WIDTH	(19*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#elif HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
#define REAL_WIDTH	(7*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#endif
#endif /* REAL_ARITHMETIC */

#ifndef REAL_WIDTH
#if HOST_BITS_PER_WIDE_INT*2 >= LONG_DOUBLE_TYPE_SIZE
#define REAL_WIDTH	2
#elif HOST_BITS_PER_WIDE_INT*3 >= LONG_DOUBLE_TYPE_SIZE
#define REAL_WIDTH	3
#elif HOST_BITS_PER_WIDE_INT*4 >= LONG_DOUBLE_TYPE_SIZE
#define REAL_WIDTH	4
#endif
#endif /* REAL_WIDTH */

#if REAL_WIDTH == 1
#define CONST_DOUBLE_FORMAT	"e0ww"
#elif REAL_WIDTH == 2
#define CONST_DOUBLE_FORMAT	"e0ww"
#elif REAL_WIDTH == 3
#define CONST_DOUBLE_FORMAT	"e0www"
#elif REAL_WIDTH == 4
#define CONST_DOUBLE_FORMAT	"e0wwww"
#elif REAL_WIDTH == 5
#define CONST_DOUBLE_FORMAT	"e0wwwww"
#else
#define CONST_DOUBLE_FORMAT	/* nothing - will cause syntax error */
#endif


struct rtx_definition 
{
  const char *enumname, *name, *format;
};

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) { STRINGIFY(ENUM), NAME, FORMAT },

struct rtx_definition defs[] = 
{  
#include "rtl.def"		/* rtl expressions are documented here */
};

const char *formats[NUM_RTX_CODE];

static const char *type_from_format	PARAMS ((int));
static const char *accessor_from_format	PARAMS ((int));
static int special_format		PARAMS ((const char *));
static int special_rtx			PARAMS ((int));
static void find_formats		PARAMS ((void));
static void gendecl			PARAMS ((const char *));
static void genmacro			PARAMS ((int));
static void gendef			PARAMS ((const char *));
static void genlegend			PARAMS ((void));
static void genheader			PARAMS ((void));
static void gencode			PARAMS ((void));

/* Decode a format letter into a C type string.  */

static const char *
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
    case 'b':
      return "struct bitmap_head_def *";  /* bitmap - typedef not available */
    case 't':
      return "union tree_node *";  /* tree - typedef not available */
    default:
      abort ();
    }
}

/* Decode a format letter into the proper accessor function.  */

static const char *
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
     const char *fmt;
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
	  || strcmp (defs[idx].enumname, "CONST_DOUBLE") == 0
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
      const char **f;

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
     const char *format;
{
  const char *p;
  int i, pos;
  
  printf ("extern rtx gen_rtx_fmt_%s\tPARAMS ((RTX_CODE, ", format);
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
  const char *p;
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
     const char *format;
{
  const char *p;
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
  printf ("  rtx rt;\n");
  printf ("  if (ggc_p)\n");
  printf ("    rt = ggc_alloc_rtx (%d);\n", 
	   (int) strlen (format));
  printf ("  else\n");
  printf ("    rt = obstack_alloc_rtx (sizeof (struct rtx_def) + %d * sizeof (rtunion));\n",
	   (int) strlen (format) - 1);

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
  const char **fmt;
  
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
  const char **fmt;

  puts ("#include \"config.h\"\n");
  puts ("#include \"system.h\"\n");
  puts ("#include \"obstack.h\"\n");
  puts ("#include \"rtl.h\"\n");
  puts ("#include \"ggc.h\"\n\n");
  puts ("extern struct obstack *rtl_obstack;\n\n");
  puts ("static rtx obstack_alloc_rtx PARAMS ((int length));\n");
  puts ("static rtx obstack_alloc_rtx (length)\n");
  puts ("     register int length;\n{\n");
  puts ("  rtx rt = (rtx) obstack_alloc (rtl_obstack, length);\n\n");
  puts ("  memset(rt, 0, sizeof(struct rtx_def) - sizeof(rtunion));\n\n");
  puts ("  return rt;\n}\n\n");

  for (fmt = formats; *fmt != 0; fmt++)
    gendef (*fmt);
}

#if defined(USE_C_ALLOCA)
PTR
xmalloc (nbytes)
  size_t nbytes;
{
  register PTR tmp = (PTR) malloc (nbytes);

  if (!tmp)
    {
      fprintf (stderr, "can't allocate %d bytes (out of virtual memory)\n",
	       nbytes);
      exit (FATAL_EXIT_CODE);
    }

  return tmp;
}
#endif /* USE_C_ALLOCA */

/* This is the main program.  We accept only one argument, "-h", which
   says we are writing the genrtl.h file.  Otherwise we are writing the
   genrtl.c file.  */
extern int main PARAMS ((int, char **));

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
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
