/* Subroutines for the gcc driver.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "configargs.h"

struct vendor_cpu
{
  const char *part_no;
  const char *arch_name;
  const char *cpu_name;
};

struct vendor
{
  const char *vendor_no;
  const struct vendor_cpu *vendor_parts;
};

#include "arm-native.h"

/* This will be called by the spec parser in gcc.cc when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "arch", "cpu" or "tune" as argument depending on if
   -march=native, -mcpu=native or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-march=armv7-a" on a Cortex-A8 for
   -march=native.  If the routine can't detect a known processor,
   the -march or -mtune option is discarded.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */
const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *val = NULL;
  char buf[128];
  FILE *f = NULL;
  bool arch;
  const struct vendor_cpu *cpu_table = NULL;
  char *fcpu_info = NULL;

  if (argc < 1)
    goto not_found;

  arch = strcmp (argv[0], "arch") == 0;
  if (!arch && strcmp (argv[0], "cpu") != 0 && strcmp (argv[0], "tune"))
    goto not_found;

  fcpu_info = getenv ("GCC_CPUINFO");
  if (fcpu_info)
    f = fopen (fcpu_info, "r");
  else
    f = fopen ("/proc/cpuinfo", "r");

  if (f == NULL)
    goto not_found;

  while (fgets (buf, sizeof (buf), f) != NULL)
    {
      /* Find the vendor table associated with this implementer.  */
      if (startswith (buf, "CPU implementer"))
	{
	  int i;
	  for (i = 0; vendors_table[i].vendor_no != NULL; i++)
	    if (strstr (buf, vendors_table[i].vendor_no) != NULL)
	      {
		cpu_table = vendors_table[i].vendor_parts;
		break;
	      }
	}

      /* Detect arch/cpu.  */
      if (startswith (buf, "CPU part"))
	{
	  int i;

	  if (cpu_table == NULL)
	    goto not_found;

	  for (i = 0; cpu_table[i].part_no != NULL; i++)
	    if (strstr (buf, cpu_table[i].part_no) != NULL)
	      {
		val = arch ? cpu_table[i].arch_name : cpu_table[i].cpu_name;
		break;
	      }
	  break;
	}
    }

  if (val)
    {
      fclose (f);
      return concat ("-m", argv[0], "=", val, NULL);
     }

not_found:
  {
    unsigned int i;
    unsigned int opt;
    const char *search[] = {NULL, "arch"};

    if (f)
      fclose (f);

    search[0] = argv[0];
    for (opt = 0; opt < ARRAY_SIZE (search); opt++)
      for (i = 0; i < ARRAY_SIZE (configure_default_options); i++)
	if (strcmp (configure_default_options[i].name, search[opt]) == 0)
	  return concat ("-m", search[opt], "=",
			 configure_default_options[i].value, NULL);
    return NULL;
  }
}
