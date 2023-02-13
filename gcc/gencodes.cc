/* Generate from machine description:
   - some macros CODE_FOR_... giving the insn_code_number value
   for each of the defined standard insn names.
   Copyright (C) 1987-2023 Free Software Foundation, Inc.

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
#include "gensupport.h"

static void
gen_insn (md_rtx_info *info)
{
  const char *name = XSTR (info->def, 0);
  int truth = maybe_eval_c_test (XSTR (info->def, 2));

  /* Don't mention instructions whose names are the null string
     or begin with '*'.  They are in the machine description just
     to be recognized.  */
  if (name[0] != 0 && name[0] != '*')
    {
      if (truth == 0)
	printf (",\n   CODE_FOR_%s = CODE_FOR_nothing", name);
      else
	printf (",\n  CODE_FOR_%s = %d", name, info->index);
    }
}

int
main (int argc, const char **argv)
{
  progname = "gencodes";

  /* We need to see all the possibilities.  Elided insns may have
     direct references to CODE_FOR_xxx in C code.  */
  insn_elision = 0;

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  printf ("\
/* Generated automatically by the program `gencodes'\n\
   from the machine description file `md'.  */\n\
\n\
#ifndef GCC_INSN_CODES_H\n\
#define GCC_INSN_CODES_H\n\
\n\
enum insn_code {\n\
  CODE_FOR_nothing = 0");

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
      case DEFINE_EXPAND:
	gen_insn (&info);
	break;

      default:
	break;
    }

  printf ("\n};\n\
\n\
const unsigned int NUM_INSN_CODES = %d;\n\
#endif /* GCC_INSN_CODES_H */\n", get_num_insn_codes ());

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
