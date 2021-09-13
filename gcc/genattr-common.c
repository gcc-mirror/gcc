/* Generate attribute information shared between driver and core
   compilers (insn-attr-common.h) from machine description.  Split out
   of genattr.c.
   Copyright (C) 1991-2021 Free Software Foundation, Inc.

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
#include "read-md.h"
#include "gensupport.h"

static void
write_upcase (const char *str)
{
  for (; *str; str++)
    putchar (TOUPPER (*str));
}

static void
gen_attr (md_rtx_info *info)
{
  const char *p, *tag;

  rtx attr = info->def;
  p = XSTR (attr, 1);
  if (*p != '\0')
    {
      printf ("enum attr_%s {", XSTR (attr, 0));

      while ((tag = scan_comma_elt (&p)) != 0)
	{
	  write_upcase (XSTR (attr, 0));
	  putchar ('_');
	  while (tag != p)
	    putchar (TOUPPER (*tag++));
	  if (*p == ',')
	    fputs (", ", stdout);
	}
      fputs ("};\n", stdout);
    }
}

int
main (int argc, const char **argv)
{
  bool have_delay = false;
  bool have_sched = false;

  progname = "genattr-common";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  puts ("/* Generated automatically by the program `genattr-common'");
  puts ("   from the machine description file `md'.  */\n");
  puts ("#ifndef GCC_INSN_ATTR_COMMON_H");
  puts ("#define GCC_INSN_ATTR_COMMON_H\n");

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_ATTR:
	gen_attr (&info);
	break;

      case DEFINE_DELAY:
	have_delay = true;
	break;

      case DEFINE_INSN_RESERVATION:
	if (!have_sched)
	  {
	    printf ("#define INSN_SCHEDULING\n");
	    have_sched = true;
	  }
	break;

      default:
	break;
      }

	    printf ("#define DELAY_SLOTS %d\n", have_delay);
  puts ("\n#endif /* GCC_INSN_ATTR_COMMON_H */");

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
