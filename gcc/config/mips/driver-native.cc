/* Subroutines for the gcc driver.
   Copyright (C) 2008-2024 Free Software Foundation, Inc.

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
#ifdef HAVE_SYS_AUXV_H
#include <sys/auxv.h>
#endif

/* This will be called by the spec parser in gcc.cc when it sees
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
  /* Don't assigne any static string to ret.  If you need to do so,
     use concat.  */
  char *ret = NULL;
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
    goto fallback_cpu;

  while (fgets (buf, sizeof (buf), f) != NULL)
    if (startswith (buf, "cpu model"))
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

fallback_cpu:
#if defined (__mips_nan2008)
  /* Put the ret to the end of list, since it may be NULL.  */
  if (arch)
    ret = reconcat (ret, " -mnan=2008 ", ret, NULL);
#endif

#ifdef HAVE_GETAUXVAL
  if (cpu == NULL)
    cpu = (const char *) getauxval (AT_BASE_PLATFORM);
#endif

#if defined (_MIPS_ARCH)
  if (cpu == NULL)
    cpu = _MIPS_ARCH;
#endif

  if (cpu)
    ret = reconcat (ret, " -m", argv[0], "=", cpu, ret, NULL);

  return ret;
}
