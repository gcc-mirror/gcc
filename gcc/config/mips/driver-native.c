/* Subroutines for the gcc driver.
   Copyright (C) 2008-2013 Free Software Foundation, Inc.

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

/* This will be called by the spec parser in gcc.c when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "arch" or "tune" as argument depending on if -march=native
   or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-march=loongson2f" on a Loongson 2F for
   -march=native.  If the routine can't detect a known processor,
   the -march or -mtune option is discarded.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */
const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu = NULL;
  char buf[128];
  FILE *f;
  bool arch;

  if (argc < 1)
    return NULL;

  arch = strcmp (argv[0], "arch") == 0;
  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

  f = fopen ("/proc/cpuinfo", "r");
  if (f == NULL)
    return NULL;

  while (fgets (buf, sizeof (buf), f) != NULL)
    if (strncmp (buf, "cpu model", sizeof ("cpu model") - 1) == 0)
      {
	if (strstr (buf, "Godson2 V0.2") != NULL
	    || strstr (buf, "Loongson-2 V0.2") != NULL
	    || strstr (buf, "Loongson-2E") != NULL)
	  cpu = "loongson2e";
	else if (strstr (buf, "Godson2 V0.3") != NULL
		 || strstr (buf, "Loongson-2 V0.3") != NULL
		 || strstr (buf, "Loongson-2F") != NULL)
	  cpu = "loongson2f";
	else if (strstr (buf, "Godson3 V0.5") != NULL
		 || strstr (buf, "Loongson-3 V0.5") != NULL
		 || strstr (buf, "Loongson-3A") != NULL)
	  cpu = "loongson3a";
	else if (strstr (buf, "SiByte SB1") != NULL)
	  cpu = "sb1";
	else if (strstr (buf, "R5000") != NULL)
	  cpu = "r5000";
	else if (strstr (buf, "Octeon II") != NULL)
	  cpu = "octeon2";
	else if (strstr (buf, "Octeon") != NULL)
	  cpu = "octeon";
	break;
      }

  fclose (f);

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
