/* Generate from machine description the strings for each enum.
   Copyright (C) 2010-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "errors.h"
#include "statistics.h"
#include "vec.h"
#include "read-md.h"

/* Called via traverse_enum_types.  Emit an enum definition for
   enum_type *SLOT.  */

static int
print_enum_type (void **slot, void *info ATTRIBUTE_UNUSED)
{
  struct enum_type *def;
  struct enum_value *value;

  def = (struct enum_type *) *slot;
  printf ("\nconst char *const %s_strings[] = {", def->name);
  for (value = def->values; value; value = value->next)
    {
      printf ("\n  \"%s\"", value->def->name);
      if (value->next)
	putc (',', stdout);
    }
  printf ("\n};\n");
  return 1;
}

int
main (int argc, const char **argv)
{
  progname = "genenums";

  noop_reader reader;
  if (!reader.read_md_files (argc, argv, NULL))
    return (FATAL_EXIT_CODE);

  puts ("/* Generated automatically by the program `genenums'");
  puts ("   from the machine description file.  */\n");
  puts ("#include \"config.h\"\n");
  puts ("#include \"system.h\"\n");
  puts ("#include \"insn-constants.h\"\n");

  reader.traverse_enum_types (print_enum_type, 0);

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
