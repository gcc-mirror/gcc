/* Generate from machine description:
   - some macros CODE_FOR_... giving the insn_code_number value
   for each of the defined standard insn names.
   Copyright (C) 1987, 1991, 1995, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.

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
#include "errors.h"
#include "gensupport.h"

static int insn_code_number;

static void gen_insn PARAMS ((rtx));

static void
gen_insn (insn)
     rtx insn;
{
  /* Don't mention instructions whose names are the null string
     or begin with '*'.  They are in the machine description just
     to be recognized.  */
  if (XSTR (insn, 0)[0] != 0 && XSTR (insn, 0)[0] != '*')
    printf ("  CODE_FOR_%s = %d,\n", XSTR (insn, 0),
	    insn_code_number);
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;

  progname = "gencodes";

  if (argc <= 1)
    fatal ("No input file name.");

  if (init_md_reader (argv[1]) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  puts ("/* Generated automatically by the program `gencodes'");
  puts ("   from the machine description file `md'.  */\n");
  puts ("#ifndef GCC_INSN_CODES_H");
  puts ("#define GCC_INSN_CODES_H\n");

  /* Read the machine description.  */

  insn_code_number = 0;
  printf ("enum insn_code {\n");

  while (1)
    {
      int line_no;

      desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;

      if (GET_CODE (desc) == DEFINE_INSN || GET_CODE (desc) == DEFINE_EXPAND)
	gen_insn (desc);
    }

  printf ("  CODE_FOR_nothing = %d };\n", insn_code_number + 1);

  printf ("\n#define MAX_INSN_CODE ((int) CODE_FOR_nothing)\n\n");

  puts("\n#endif /* GCC_INSN_CODES_H */");

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */

const char *
get_insn_name (code)
     int code ATTRIBUTE_UNUSED;
{
  return NULL;
}
