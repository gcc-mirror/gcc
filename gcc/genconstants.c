/* Generate from machine description:
   a series of #define statements, one for each constant named in
   a (define_constants ...) pattern.

   Copyright (C) 1987, 1991, 1995, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This program does not use gensupport.c because it does not need to
   look at insn patterns, only (define_constants), and we want to
   minimize dependencies.  */

#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "errors.h"
#include "gensupport.h"

static int print_md_constant PARAMS ((void **, void *));
extern int main PARAMS ((int, char **));

/* Called via traverse_md_constants; emit a #define for
   the current constant definition.  */

static int
print_md_constant (slot, info)
     void **slot;
     void *info;
{
  struct md_constant *def = *slot;
  FILE *file = info;

  fprintf (file, "#define %s %s\n", def->name, def->value);
  return 1;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int dummy1, dummy2;
  rtx desc;

  progname = "genconstants";

  if (argc <= 1)
    fatal ("no input file name");

  if (init_md_reader (argv[1]) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  /* Scan and discard the entire file.  This has the side effect
     of loading up the constants table that we wish to scan.  */
  do
    desc = read_md_rtx (&dummy1, &dummy2);
  while (desc);

  puts ("/* Generated automatically by the program `genconstants'");
  puts ("   from the machine description file `md'.  */\n");
  puts ("#ifndef GCC_INSN_CONSTANTS_H");
  puts ("#define GCC_INSN_CONSTANTS_H\n");

  traverse_md_constants (print_md_constant, stdout);

  puts ("\n#endif /* GCC_INSN_CONSTANTS_H */");

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}

