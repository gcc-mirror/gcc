/* Subroutines for the gcc driver.
   Copyright (C) 2009 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

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
  const char *cpu = NULL;
  char buf[128];
  FILE *f;

  static const struct cpu_names {
   const char *const name;
   const char *const cpu;
  } cpu_names[] = {
    { "EV79",	"ev67" },
    { "EV7",	"ev67" },
    { "EV69",	"ev67" },
    { "EV68CX",	"ev67" },
    { "EV68CB",	"ev67" },
    { "EV68AL",	"ev67" },
    { "EV67",	"ev67" },
    { "EV6",	"ev6" },
    { "PCA57",	"pca56" },
    { "PCA56",	"pca56" },
    { "EV56",	"ev56" },
    { "EV5",	"ev5" },
    { "LCA45",	"ev45" },
    { "EV45",	"ev45" },
    { "LCA4",	"ev4" },
    { "EV4",	"ev4" },
/*  { "EV3",	"ev3" },  */
    { 0, 0 }
  };

  int i;

  if (argc < 1)
    return NULL;

  if (strcmp (argv[0], "cpu") && strcmp (argv[0], "tune"))
    return NULL;

  f = fopen ("/proc/cpuinfo", "r");
  if (f == NULL)
    return NULL;

  while (fgets (buf, sizeof (buf), f) != NULL)
    if (strncmp (buf, "cpu model", sizeof ("cpu model") - 1) == 0)
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

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
