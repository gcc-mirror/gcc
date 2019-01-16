/* Subroutines for the gcc driver.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by Arthur Loiret <aloiret@debian.org>

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

/* Chip family type IDs, returned by implver instruction.  */
#define IMPLVER_EV4_FAMILY	0		/* LCA/EV4/EV45 */
#define IMPLVER_EV5_FAMILY	1		/* EV5/EV56/PCA56 */
#define IMPLVER_EV6_FAMILY	2		/* EV6 */
#define IMPLVER_EV7_FAMILY	3		/* EV7 */

/* Bit defines for amask instruction.  */
#define AMASK_BWX          0x1          /* byte/word extension.  */
#define AMASK_FIX          0x2          /* sqrt and f <-> i conversions 
					   extension.  */
#define AMASK_CIX          0x4          /* count extension.  */
#define AMASK_MVI          0x100        /* multimedia extension.  */
#define AMASK_PRECISE      0x200        /* Precise arithmetic traps.  */
#define AMASK_LOCKPFTCHOK  0x1000       /* Safe to prefetch lock cache
					   block.  */

/* This will be called by the spec parser in gcc.c when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "cpu" or "tune" as argument depending on if -mcpu=native
   or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-mcpu=ev6" on an Alpha 21264 for
   -mcpu=native.  If the routine can't detect a known processor,
   the -mcpu or -mtune option is discarded.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */
const char *
host_detect_local_cpu (int argc, const char **argv)
{
  static const struct cpu_types {
    long implver;
    long amask;
    const char *const cpu;
  } cpu_types[] = {
    { IMPLVER_EV7_FAMILY, AMASK_BWX|AMASK_MVI|AMASK_FIX|AMASK_CIX, "ev67" },
    { IMPLVER_EV6_FAMILY, AMASK_BWX|AMASK_MVI|AMASK_FIX|AMASK_CIX, "ev67" },
    { IMPLVER_EV6_FAMILY, AMASK_BWX|AMASK_MVI|AMASK_FIX, "ev6" },
    { IMPLVER_EV5_FAMILY, AMASK_BWX|AMASK_MVI, "pca56" },
    { IMPLVER_EV5_FAMILY, AMASK_BWX, "ev56" },
    { IMPLVER_EV5_FAMILY, 0, "ev5" },
    { IMPLVER_EV4_FAMILY, 0, "ev4" },
    { 0, 0, NULL }
  };
  long implver;
  long amask;
  const char *cpu;
  int i;

  if (argc < 1)
    return NULL;

  if (strcmp (argv[0], "cpu") && strcmp (argv[0], "tune"))
    return NULL;

  implver = __builtin_alpha_implver ();
  amask = __builtin_alpha_amask (~0L);
  cpu = NULL;

  for (i = 0; cpu_types[i].cpu != NULL; i++)
    if (implver == cpu_types[i].implver
	&& (~amask & cpu_types[i].amask) == cpu_types[i].amask)
      {
	cpu = cpu_types[i].cpu;
	break;
      }

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
