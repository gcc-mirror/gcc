/* Generate code to allocate RTL structures.
   Copyright (C) 1997-2025 Free Software Foundation, Inc.

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

struct rtx_definition
{
  const char *const enumname, *const name, *const format;
};

/* rtl.def needs CONST_DOUBLE_FORMAT, but we don't care what
   CONST_DOUBLE_FORMAT is because we're not going to be generating
   anything for CONST_DOUBLE anyway.  */
#define CONST_DOUBLE_FORMAT ""

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) { #ENUM, NAME, FORMAT },

static const struct rtx_definition defs[] =
{
#include "rtl.def"		/* rtl expressions are documented here */
};
#define NUM_RTX_CODE ARRAY_SIZE (defs)

static const char *formats[NUM_RTX_CODE];

/* Decode a format letter into a C type string.  */

static const char *
type_from_format (int c)
{
  switch (c)
    {
    case 'i':
      return "int ";

    case 'L':
      return "location_t ";

    case 'w':
      return "HOST_WIDE_INT ";

    case 'p':
      return "poly_uint16 ";

    case 's':
      return "const char *";

    case 'e':  case 'u':
      return "rtx ";

    case 'E':
      return "rtvec ";
    case 't':
      return "tree ";
    case 'B':
      return "basic_block ";
    default:
      gcc_unreachable ();
    }
}

/* Decode a format letter into the proper accessor function.  */

static const char *
accessor_from_format (int c)
{
  switch (c)
    {
    case 'i':
      return "XINT";

    case 'L':
      return "XLOC";

    case 'w':
      return "XWINT";

    case 's':
      return "XSTR";

    case 'e':  case 'u':
      return "XEXP";

    case 'E':
      return "XVEC";

    case 't':
      return "XTREE";

    case 'B':
      return "XBBDEF";

    default:
      gcc_unreachable ();
    }
}

/* Return nonzero if we should ignore FMT, an RTL format, when making
   the list of formats we write routines to create.  */

static int
special_format (const char *fmt)
{
  return (strchr (fmt, '*') != 0
	  || strchr (fmt, 'V') != 0
	  || strchr (fmt, 'S') != 0
	  || strchr (fmt, 'n') != 0
	  || strchr (fmt, 'r') != 0);
}

/* Return true if CODE always has VOIDmode.  */

static inline bool
always_void_p (int idx)
{
  return strcmp (defs[idx].enumname, "SET") == 0;
}

/* Return nonzero if the RTL code given by index IDX is one that we should
   generate a gen_rtx_raw_FOO macro for, not gen_rtx_FOO (because gen_rtx_FOO
   is a wrapper in emit-rtl.cc).  */

static int
special_rtx (int idx)
{
  return (strcmp (defs[idx].enumname, "EXPR_LIST") == 0
	  || strcmp (defs[idx].enumname, "INSN_LIST") == 0
	  || strcmp (defs[idx].enumname, "INSN") == 0
	  || strcmp (defs[idx].enumname, "CONST_INT") == 0
	  || strcmp (defs[idx].enumname, "REG") == 0
	  || strcmp (defs[idx].enumname, "SUBREG") == 0
	  || strcmp (defs[idx].enumname, "MEM") == 0
	  || strcmp (defs[idx].enumname, "PC") == 0
	  || strcmp (defs[idx].enumname, "RETURN") == 0
	  || strcmp (defs[idx].enumname, "SIMPLE_RETURN") == 0
	  || strcmp (defs[idx].enumname, "CONST_VECTOR") == 0);
}

/* Return nonzero if the RTL code given by index IDX is one that we should
   generate no macro for at all (because gen_rtx_FOO is never used or
   cannot have the obvious interface).  */

static int
excluded_rtx (int idx)
{
  return (strcmp (defs[idx].enumname, "VAR_LOCATION") == 0
	  || strcmp (defs[idx].enumname, "CONST_DOUBLE") == 0
	  || strcmp (defs[idx].enumname, "CONST_WIDE_INT") == 0
	  || strcmp (defs[idx].enumname, "CONST_POLY_INT") == 0
	  || strcmp (defs[idx].enumname, "CONST_FIXED") == 0);
}

/* Place a list of all format specifiers we use into the array FORMAT.  */

static void
find_formats (void)
{
  unsigned int i;

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


/* Generate macros to generate RTL of code IDX using the functions we
   write.  */

static void
genmacro (int idx)
{
  const char *p;
  const char *sep = "";
  int i;

  /* We write a macro that defines gen_rtx_RTLCODE to be an equivalent to
     gen_rtx_fmt_FORMAT where FORMAT is the RTX_FORMAT of RTLCODE.  */

  if (excluded_rtx (idx))
    /* Don't define a macro for this code.  */
    return;

  bool has_mode_p = !always_void_p (idx);
  printf ("#define gen_rtx_%s%s(",
	   special_rtx (idx) ? "raw_" : "", defs[idx].enumname);
  if (has_mode_p)
    {
      printf ("MODE");
      sep = ", ";
    }

  for (p = defs[idx].format, i = 0; *p != 0; p++)
    if (*p != '0')
      {
	printf ("%sARG%d", sep, i++);
	sep = ", ";
      }

  printf (") \\\n  gen_rtx_fmt_%s (%s, %s",
	  defs[idx].format, defs[idx].enumname,
	  has_mode_p ? "(MODE)" : "VOIDmode");

  for (p = defs[idx].format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", (ARG%d)", i++);

  puts (")");
}

/* Generate the code for functions to generate RTL whose format is FORMAT.  */

static void
gendef (const char *format)
{
  const char *p;
  int i, j;

  /* Write the definition of the init function name and the types
     of the arguments.  */

  puts ("static inline rtx");
  printf ("init_rtx_fmt_%s (rtx rt, machine_mode mode", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (",\n\t%sarg%d", type_from_format (*p), i++);
  puts (")");

  /* Now write out the body of the init function itself.  */
  puts ("{");
  puts ("  PUT_MODE_RAW (rt, mode);");

  for (p = format, i = j = 0; *p ; ++p, ++i)
    if (*p == '0')
      printf ("  X0EXP (rt, %d) = NULL_RTX;\n", i);
    else if (*p == 'p')
      printf ("  SUBREG_BYTE (rt) = arg%d;\n", j++);
    else
      printf ("  %s (rt, %d) = arg%d;\n", accessor_from_format (*p), i, j++);

  puts ("  return rt;\n}\n");

  /* Write the definition of the gen function name and the types
     of the arguments.  */

  puts ("static inline rtx");
  printf ("gen_rtx_fmt_%s_stat (RTX_CODE code, machine_mode mode", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (",\n\t%sarg%d", type_from_format (*p), i++);
  puts (" MEM_STAT_DECL)");

  /* Now write out the body of the function itself, which allocates
     the memory and initializes it.  */
  puts ("{");
  puts ("  rtx rt;\n");

  puts ("  rt = rtx_alloc (code PASS_MEM_STAT);");
  printf ("  return init_rtx_fmt_%s (rt, mode", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", arg%d", i++);
  puts (");\n}\n");

  /* Write the definition of gen macro.  */

  printf ("#define gen_rtx_fmt_%s(c, m", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", arg%d", i++);
  printf (") \\\n  gen_rtx_fmt_%s_stat ((c), (m)", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", (arg%d)", i++);
  printf (" MEM_STAT_INFO)\n\n");

  /* Write the definition of alloca macro.  */

  printf ("#define alloca_rtx_fmt_%s(c, m", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", arg%d", i++);
  printf (") \\\n  init_rtx_fmt_%s (rtx_alloca ((c)), (m)", format);
  for (p = format, i = 0; *p != 0; p++)
    if (*p != '0')
      printf (", (arg%d)", i++);
  printf (")\n\n");
}

/* Generate the documentation header for files we write.  */

static void
genlegend (void)
{
  puts ("/* Generated automatically by gengenrtl from rtl.def.  */\n");
}

/* Generate the text of the header file we make, genrtl.h.  */

static void
genheader (void)
{
  unsigned int i;
  const char **fmt;

  puts ("#ifndef GCC_GENRTL_H");
  puts ("#define GCC_GENRTL_H\n");
  puts ("#include \"statistics.h\"\n");

  for (fmt = formats; *fmt; ++fmt)
    gendef (*fmt);

  putchar ('\n');

  for (i = 0; i < NUM_RTX_CODE; i++)
    if (! special_format (defs[i].format))
      genmacro (i);

  puts ("\n#endif /* GCC_GENRTL_H */");
}

/* This is the main program.  */

int
main (void)
{
  find_formats ();
  genlegend ();

  genheader ();

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
