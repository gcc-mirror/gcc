/* Generate code to allocate RTL structures.
   Copyright (C) 1997 Free Software Foundation, Inc.

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
#include <stdio.h>

#include "obstack.h"
#define	obstack_chunk_alloc	xmalloc
#define	obstack_chunk_free	free

#define NO_GENRTL_H
#include "rtl.h"


struct rtx_definition 
{
  const char *enumname, *name, *format;
};

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) { # ENUM, NAME, FORMAT },

struct rtx_definition defs[] = 
{  
#include "rtl.def"		/* rtl expressions are documented here */
};

const char *formats[NUM_RTX_CODE];

static const char *
type_from_format (char c)
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
    default:
      abort ();
    }
}

static const char *
accessor_from_format (char c)
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
    default:
      abort ();
    }
}

static int
special_format (fmt)
     const char *fmt;
{
  return (strchr (fmt, '*') != 0
	  || strchr (fmt, 'V') != 0
	  || strchr (fmt, 'S') != 0
	  || strchr (fmt, 'n') != 0);
}

static int
special_rtx (idx)
     int idx;
{
  return (strcmp (defs[idx].enumname, "CONST_INT") == 0
	  || strcmp (defs[idx].enumname, "REG") == 0);
}

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
	if (!strcmp(*f, defs[i].format))
	  break;

      if (!*f)
	*f = defs[i].format;
    }
}

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
  fprintf (f, "  register int length = sizeof (struct rtx_def)");
  fprintf (f, " + %d * sizeof (rtunion);\n", strlen (format) - 1);
  fprintf (f, "  rtx rt = (rtx)obstack_alloc (rtl_obstack, length);\n");

  fprintf (f, "  if (sizeof(struct rtx_def) - sizeof(rtunion) == sizeof(int))\n");
  fprintf (f, "    *(int *)rt = 0;\n");
  fprintf (f, "  else if (sizeof(struct rtx_def) - sizeof(rtunion) == sizeof(HOST_WIDE_INT))\n");
  fprintf (f, "    *(HOST_WIDE_INT *)rt = 0;\n");
  fprintf (f, "  else\n");
  fprintf (f, "    bzero(rt, sizeof(struct rtx_def) - sizeof(rtunion));\n\n");

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

static void
genlegend (f)
     FILE *f;
{
  fprintf (f, "/* Generated automaticaly by the program `gengenrtl'\n");
  fprintf (f, "   from the RTL description file `rtl.def' */\n\n");
}

static void
genheader (f)
     FILE *f;
{
  int i;
  const char **fmt;

  for (fmt = formats; *fmt; ++fmt)
    gendecl (f, *fmt);

  fprintf(f, "\n");

  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      if (special_format (defs[i].format))
	continue;
      genmacro (f, i);
    }
}

static void
gencode (f)
     FILE *f;
{
  const char **fmt;

  fprintf(f, "#include \"config.h\"\n");
  fprintf(f, "#include \"obstack.h\"\n");
  fprintf(f, "#include \"rtl.h\"\n\n");
  fprintf(f, "extern struct obstack *rtl_obstack;\n\n");

  for (fmt = formats; *fmt; ++fmt)
    gendef (f, *fmt);
}

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
      perror(argv[1]);
      exit (1);
    }
  genlegend (f);
  genheader (f);
  fclose(f);

  f = fopen (argv[2], "w");
  if (f == NULL)
    {
      perror(argv[2]);
      exit (1);
    }
  genlegend (f);
  gencode (f);
  fclose(f);

  exit (0);
}
