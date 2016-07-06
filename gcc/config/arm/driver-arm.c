/* Subroutines for the gcc driver.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "configargs.h"

struct vendor_cpu {
  const char *part_no;
  const char *arch_name;
  const char *cpu_name;
};

static struct vendor_cpu arm_cpu_table[] = {
    {"0x926", "armv5te", "arm926ej-s"},
    {"0xa26", "armv5te", "arm1026ej-s"},
    {"0xb02", "armv6k", "mpcore"},
    {"0xb36", "armv6j", "arm1136j-s"},
    {"0xb56", "armv6t2", "arm1156t2-s"},
    /* armv6kz is the correct spelling for ARMv6KZ but may not be supported in
       the version of binutils used.  The incorrect spelling is supported in
       legacy and current binutils so that is used instead.  */
    {"0xb76", "armv6zk", "arm1176jz-s"},
    {"0xc05", "armv7-a", "cortex-a5"},
    {"0xc07", "armv7ve", "cortex-a7"},
    {"0xc08", "armv7-a", "cortex-a8"},
    {"0xc09", "armv7-a", "cortex-a9"},
    {"0xc0d", "armv7ve", "cortex-a12"},
    {"0xc0e", "armv7ve", "cortex-a17"},
    {"0xc0f", "armv7ve", "cortex-a15"},
    {"0xd01", "armv8-a+crc", "cortex-a32"},
    {"0xd04", "armv8-a+crc", "cortex-a35"},
    {"0xd03", "armv8-a+crc", "cortex-a53"},
    {"0xd07", "armv8-a+crc", "cortex-a57"},
    {"0xd08", "armv8-a+crc", "cortex-a72"},
    {"0xd09", "armv8-a+crc", "cortex-a73"},
    {"0xc14", "armv7-r", "cortex-r4"},
    {"0xc15", "armv7-r", "cortex-r5"},
    {"0xc20", "armv6-m", "cortex-m0"},
    {"0xc21", "armv6-m", "cortex-m1"},
    {"0xc23", "armv7-m", "cortex-m3"},
    {"0xc24", "armv7e-m", "cortex-m4"},
    {NULL, NULL, NULL}
};

static struct {
  const char *vendor_no;
  const struct vendor_cpu *vendor_parts;
} vendors[] = {
    {"0x41", arm_cpu_table},
    {NULL, NULL}
};

/* This will be called by the spec parser in gcc.c when it sees
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

  if (argc < 1)
    goto not_found;

  arch = strcmp (argv[0], "arch") == 0;
  if (!arch && strcmp (argv[0], "cpu") != 0 && strcmp (argv[0], "tune"))
    goto not_found;

  f = fopen ("/proc/cpuinfo", "r");
  if (f == NULL)
    goto not_found;

  while (fgets (buf, sizeof (buf), f) != NULL)
    {
      /* Ensure that CPU implementer is ARM (0x41).  */
      if (strncmp (buf, "CPU implementer", sizeof ("CPU implementer") - 1) == 0)
	{
	  int i;
	  for (i = 0; vendors[i].vendor_no != NULL; i++)
	    if (strstr (buf, vendors[i].vendor_no) != NULL)
	      {
		cpu_table = vendors[i].vendor_parts;
		break;
	      }
	}

      /* Detect arch/cpu.  */
      if (strncmp (buf, "CPU part", sizeof ("CPU part") - 1) == 0)
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
