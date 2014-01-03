/* Subroutines for the gcc driver.
   Copyright (C) 2011-2014 Free Software Foundation, Inc.

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

static const struct cpu_names {
  const char *const name;
  const char *const cpu;
} cpu_names[] = {
#if defined __sun__ && defined __svr4__
  { "TMS390S10",	"supersparc" },	/* Texas Instruments microSPARC I */
  { "TMS390Z50",	"supersparc" },	/* Texas Instruments SuperSPARC I */
  { "TMS390Z55",	"supersparc" },	/* Texas Instruments
					   SuperSPARC I with SuperCache */
  { "MB86904",		"supersparc" },	/* Fujitsu microSPARC II */
  { "MB86907",		"supersparc" },	/* Fujitsu TurboSPARC */
  { "RT623",		"hypersparc" },	/* Ross hyperSPARC */
  { "RT625",		"hypersparc" },
  { "RT626",		"hypersparc" },
  { "UltraSPARC-I",	"ultrasparc" },
  { "UltraSPARC-II",	"ultrasparc" },
  { "UltraSPARC-IIe",	"ultrasparc" },
  { "UltraSPARC-IIi",	"ultrasparc" },
  { "SPARC64-III",	"ultrasparc" },
  { "SPARC64-IV",	"ultrasparc" },
  { "UltraSPARC-III",	"ultrasparc3" },
  { "UltraSPARC-III+",	"ultrasparc3" },
  { "UltraSPARC-IIIi",	"ultrasparc3" },
  { "UltraSPARC-IIIi+",	"ultrasparc3" },
  { "UltraSPARC-IV",	"ultrasparc3" },
  { "UltraSPARC-IV+",	"ultrasparc3" },
  { "SPARC64-V",	"ultrasparc3" },
  { "SPARC64-VI",	"ultrasparc3" },
  { "SPARC64-VII",	"ultrasparc3" },
  { "UltraSPARC-T1",	"niagara" },
  { "UltraSPARC-T2",	"niagara2" },
  { "UltraSPARC-T2",	"niagara2" },
  { "UltraSPARC-T2+",	"niagara2" },
  { "SPARC-T3",		"niagara3" },
  { "SPARC-T4",		"niagara4" },
#else
  { "SuperSparc",	"supersparc" },
  { "HyperSparc",	"hypersparc" },
  { "SpitFire",		"ultrasparc" },
  { "BlackBird",	"ultrasparc" },
  { "Sabre",		"ultrasparc" },
  { "Hummingbird",	"ultrasparc" },
  { "Cheetah",		"ultrasparc3" },
  { "Jalapeno",		"ultrasparc3" },
  { "Jaguar",		"ultrasparc3" },
  { "Panther",		"ultrasparc3" },
  { "Serrano",		"ultrasparc3" },
  { "UltraSparc T1",	"niagara" },
  { "UltraSparc T2",	"niagara2" },
  { "UltraSparc T3",	"niagara3" },
  { "UltraSparc T4",	"niagara4" },
#endif
  { NULL,	NULL }
  };

#if defined __sun__ && defined __svr4__
#include <kstat.h>
#endif

/* This will be called by the spec parser in gcc.c when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "cpu" or "tune" as argument depending on if -mcpu=native
   or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-mcpu=ultrasparc3" on an UltraSPARC III for
   -mcpu=native.  If the routine can't detect a known processor,
   the -mcpu or -mtune option is discarded.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */
const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu = NULL;
#if defined __sun__ && defined __svr4__
  char *buf = NULL;
  kstat_ctl_t *kc;
  kstat_t *ksp;
  kstat_named_t *brand = NULL;
#else
  char buf[128];
  FILE *f;
#endif
  int i;

  if (argc < 1)
    return NULL;

  if (strcmp (argv[0], "cpu") && strcmp (argv[0], "tune"))
    return NULL;

#if defined __sun__ && defined __svr4__
  kc = kstat_open ();
  if (kc != NULL)
    {
      ksp = kstat_lookup (kc, CONST_CAST2 (char *, const char *, "cpu_info"),
			  -1, NULL);
      if (ksp != NULL
	  && kstat_read (kc, ksp, NULL) != -1
	  && ksp->ks_type == KSTAT_TYPE_NAMED)
	brand = (kstat_named_t *)
	  kstat_data_lookup (ksp, CONST_CAST2 (char *, const char *, "brand"));
      /* "brand" was only introduced in Solaris 10.  */
      if (brand == NULL)
	  brand = (kstat_named_t *)
	    kstat_data_lookup (ksp, CONST_CAST2 (char *, const char *,
						 "implementation"));
      /* KSTAT_DATA_STRING was introduced in Solaris 9.  */
#ifdef KSTAT_DATA_STRING
      if (brand != NULL && brand->data_type == KSTAT_DATA_STRING)
	buf = KSTAT_NAMED_STR_PTR (brand);
#else
      if (brand != NULL && brand->data_type == KSTAT_DATA_CHAR)
	buf = brand->value.c;
#endif
    }
  kstat_close (kc);

  for (i = 0; cpu_names[i].name != NULL; i++)
    if (strcmp (buf, cpu_names[i].name) == 0)
      cpu = cpu_names[i].cpu;
#else
  f = fopen ("/proc/cpuinfo", "r");
  if (f == NULL)
    return NULL;

  while (fgets (buf, sizeof (buf), f) != NULL)
    if (strncmp (buf, "cpu\t\t:", sizeof ("cpu\t\t:") - 1) == 0)
      {
        for (i = 0; cpu_names [i].name; i++)
          if (strstr (buf, cpu_names [i].name) != NULL)
	    {
	      cpu = cpu_names [i].cpu;
	      break;
	    }
	break;
      }

  fclose (f);
#endif

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
