/* Generate from machine description:
   - some macros CODE_FOR_... giving the insn_code_number value
   for each of the defined standard insn names.
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

#include "hconfig.h"
#include "system.h"

#define NO_GENRTL_H
#include "rtl.h"

static void output_predicate_decls PARAMS ((void));
extern int main PARAMS ((void));

static void
output_predicate_decls ()
{
#ifdef PREDICATE_CODES
  static const struct {
    const char *const name;
    const RTX_CODE codes[NUM_RTX_CODE];
  } predicate[] = {
    PREDICATE_CODES
  };
  size_t i;

  puts ("#ifdef RTX_CODE\n");
  for (i = 0; i < sizeof predicate / sizeof *predicate; i++)
    printf ("extern int %s PARAMS ((rtx, enum machine_mode));\n",
	    predicate[i].name);
  puts ("\n#endif /* RTX_CODE */\n");
#endif
}

int
main ()
{
  puts ("/* Generated automatically by the program `genpreds'.  */\n");
  puts ("#ifndef GCC_TM_PREDS_H");
  puts ("#define GCC_TM_PREDS_H\n");

  output_predicate_decls ();

  puts ("#endif /* GCC_TM_PREDS_H */");

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
