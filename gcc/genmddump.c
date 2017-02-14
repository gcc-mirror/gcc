/* Generate code from machine description to recognize rtl as insns.
   Copyright (C) 1987-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* This program is used to produce tmp-mddump.md, which represents
   md-file with expanded iterators and after define_subst transformation
   is performed.

   The only argument of the program is a source md-file (e.g.
   config/i386/i386.md).  STDERR is used for the program output.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"


extern int main (int, const char **);

int
main (int argc, const char **argv)
{
  progname = "genmddump";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  /* Read the machine description.  */
  md_rtx_info info;
  while (read_md_rtx (&info))
    {
      printf (";; %s: %d\n", info.loc.filename, info.loc.lineno);
      print_inline_rtx (stdout, info.def, 0);
      printf ("\n\n");
    }

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
