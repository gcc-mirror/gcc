/* Generate code to allocate RTL structures.
   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.

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
#undef abort

#define NO_GENRTL_H
#include "rtl.h"


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

static const char *type_from_format PROTO((int));
static const char *accessor_from_format PROTO((int));
static int special_format PROTO((const char *));
static int special_rtx PROTO((int));
static void find_formats PROTO((void));
static void gendecl PROTO((FILE *, const char *));
static void genmacro PROTO((FILE *, int));
static void gendef PROTO((FILE *, const char *));
static void genlegend PROTO((FILE *));
static void genheader PROTO((FILE *));
static void gencode PROTO((FILE *));

/* Decode a format letter into a C type string.  */

static const char *
type_from_format (c)
     int c;
{
  switch (c)
    {
    case 'i':
      return "int";
    case 'w':
      return "HOST_WIDE_INT";
    case 's':
      return "char *";
    case 'e':
    case 'u':
      return "rtx";
    case 'E':
      return "rtvec";
    /* ?!? These should be bitmap and tree respectively, but those types
       are not available in many of the files which include the output
       of gengenrtl.

       These are only used in prototypes, so I think we can assume that
       void * is useable.  */
    case 'b':
      return "void *";
    case 't':
      return "void *";
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
    case 'e':
    case 'u':
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

/* Return true if a format character doesn't need normal processing.  */

static int
special_format (fmt)
     const char *fmt;
{
  return (strchr (fmt, '*') != 0
	  || strchr (fmt, 'V') != 0
	  || strchr (fmt, 'S') != 0
	  || strchr (fmt, 'n') != 0);
}

/* Return true if an rtx requires special processing.  */

static int
special_rtx (idx)
     int idx;
{
  return (strcmp (defs[idx].enumname, "CONST_INT") == 0
	  || strcmp (defs[idx].enumname, "CONST_DOUBLE") == 0
	  || strcmp (defs[idx].enumname, "REG") == 0
	  || strcmp (defs[idx].enumname, "MEM") == 0);
}

/* Fill `formats' with all unique format strings.  */

static void
find_formats ()
{
  int i;

  for (i = 0; i < NUM_RTX_CODE; ++i)
    {
      const char **f;

      if (special_format (defs[i].format))
	continue;

      for (f = formats; *f ; ++f)
	if (! strcmp (*f, defs[i].format))
	  break;

      if (!*f)
	*f = defs[i].format;
    }
}

/* Emit a prototype for the rtx generator for a format.  */

static void
gendecl (f, format)
     FILE *f;
     const char *format;
{
  const char *p;
  int i;
  
  fprintf (f, "extern rtx gen_rtx_fmt_%s PROTO((RTX_CODE, enum machine_mode mode",
	   format);
  for (p = format, i = 0; *p ; ++p)
    if (*p != '0')
      fprintf (f, ", %s arg%d", type_from_format (*p), i++);
  fprintf (f, "));\n");
}

/* Emit a define mapping an rtx code to the generator for its format.  */

static void 
genmacro (f, idx)
     FILE *f;
     int idx;
{
  const char *p;
  int i;

  fprintf (f, "#define gen_rtx_%s%s(mode",
	   (special_rtx (idx) ? "raw_" : ""), defs[idx].enumname);

  for (p = defs[idx].format, i = 0; *p ; ++p)
    if (*p != '0')
      fprintf (f, ", arg%d", i++);
  fprintf (f, ")   ");

  fprintf (f, "gen_rtx_fmt_%s(%s,(mode)", defs[idx].format, defs[idx].enumname);
  for (p = defs[idx].format, i = 0; *p ; ++p)
    if (*p != '0')
      fprintf (f, ",(arg%d)", i++);
  fprintf (f, ")\n");
}

/* Emit the implementation for the rtx generator for a format.  */

static void
gendef (f, format)
     FILE *f;
     const char *format;
{
  const char *p;
  int i, j;
  
  fprintf (f, "rtx\ngen_rtx_fmt_%s (code, mode", format);
  for (p = format, i = 0; *p ; ++p)
    if (*p != '0')
      fprintf (f, ", arg%d", i++);

  fprintf (f, ")\n     RTX_CODE code;\n     enum machine_mode mode;\n");
  for (p = format, i = 0; *p ; ++p)
    if (*p != '0')
      fprintf (f, "     %s arg%d;\n", type_from_format (*p), i++);

  /* See rtx_alloc in rtl.c for comments.  */
  fprintf (f, "{\n");
  fprintf (f, "  rtx rt = obstack_alloc_rtx (sizeof (struct rtx_def) + %d * sizeof (rtunion));\n",
	   (int) strlen (format) - 1);

  fprintf (f, "  PUT_CODE (rt, code);\n");
  fprintf (f, "  PUT_MODE (rt, mode);\n");

  for (p = format, i = j = 0; *p ; ++p, ++i)
    if (*p != '0')
      {
	fprintf (f, "  %s (rt, %d) = arg%d;\n",
		 accessor_from_format (*p), i, j++);
      }

  fprintf (f, "\n  return rt;\n}\n\n");
}

/* Emit the `do not edit' banner.  */

static void
genlegend (f)
     FILE *f;
{
  fputs ("/* Generated automaticaly by the program `gengenrtl'\n", f);
  fputs ("   from the RTL description file `rtl.def' */\n\n", f);
}

/* Emit "genrtl.h".  */

static void
genheader (f)
     FILE *f;
{
  int i;
  const char **fmt;

  for (fmt = formats; *fmt; ++fmt)
    gendecl (f, *fmt);

  fprintf (f, "\n");

  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      if (special_format (defs[i].format))
	continue;
      genmacro (f, i);
    }
}

/* Emit "genrtl.c".  */

static void
gencode (f)
     FILE *f;
{
  const char **fmt;

  fputs ("#include \"config.h\"\n", f);
  fputs ("#include \"system.h\"\n", f);
  fputs ("#include \"obstack.h\"\n", f);
  fputs ("#include \"rtl.h\"\n\n", f);
  fputs ("extern struct obstack *rtl_obstack;\n\n", f);
  fputs ("static rtx obstack_alloc_rtx PROTO((int length));\n", f);
  fputs ("static rtx obstack_alloc_rtx (length)\n", f);
  fputs ("     register int length;\n{\n", f);
  fputs ("  rtx rt = (rtx) obstack_alloc (rtl_obstack, length);\n\n", f);
  fputs ("  memset(rt, 0, sizeof(struct rtx_def) - sizeof(rtunion));\n\n", f);
  fputs ("  return rt;\n}\n\n", f);

  for (fmt = formats; *fmt; ++fmt)
    gendef (f, *fmt);
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

int
main(argc, argv)
     int argc;
     char **argv;
{
  FILE *f;

  if (argc != 3)
    exit (1);

  find_formats ();

  f = fopen (argv[1], "w");
  if (f == NULL)
    {
      perror (argv[1]);
      exit (1);
    }
  genlegend (f);
  genheader (f);
  fclose (f);

  f = fopen (argv[2], "w");
  if (f == NULL)
    {
      perror (argv[2]);
      exit (1);
    }
  genlegend (f);
  gencode (f);
  fclose (f);

  exit (0);
}
