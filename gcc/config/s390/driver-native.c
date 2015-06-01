/* Subroutines for the gcc driver.
   Copyright (C) 2015 Free Software Foundation, Inc.

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
   this is executed.  E.g. "-march=zEC12" on a zEC12 for -march=native.
   If the routine can't detect a known processor, the -march or -mtune
   option is discarded.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */
const char *
s390_host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu = NULL;
  char buf[256];
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
    if (strncmp (buf, "processor", sizeof ("processor") - 1) == 0)
      {
	if (strstr (buf, "machine = 9672") != NULL)
	  cpu = "g5";
	else if (strstr (buf, "machine = 2064") != NULL
		 || strstr (buf, "machine = 2066") != NULL)
	  cpu = "z900";
	else if (strstr (buf, "machine = 2084") != NULL
		 || strstr (buf, "machine = 2086") != NULL)
	  cpu = "z990";
	else if (strstr (buf, "machine = 2094") != NULL
		 || strstr (buf, "machine = 2096") != NULL)
	  cpu = "z9-109";
	else if (strstr (buf, "machine = 2097") != NULL
		 || strstr (buf, "machine = 2098") != NULL)
	  cpu = "z10";
	else if (strstr (buf, "machine = 2817") != NULL
		 || strstr (buf, "machine = 2818") != NULL)
	  cpu = "z196";
	else if (strstr (buf, "machine = 2827") != NULL
		 || strstr (buf, "machine = 2828") != NULL)
	  cpu = "zEC12";
	else if (strstr (buf, "machine = 2964") != NULL)
	  cpu = "z13";
	break;
      }

  fclose (f);

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
