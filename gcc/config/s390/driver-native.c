/* Subroutines for the gcc driver.
   Copyright (C) 2015-2018 Free Software Foundation, Inc.

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
  const char *options = "";
  unsigned int has_features;
  unsigned int has_processor;
  unsigned int is_cpu_z9_109 = 0;
  unsigned int has_highgprs = 0;
  unsigned int has_dfp = 0;
  unsigned int has_te = 0;
  unsigned int has_vx = 0;
  unsigned int has_opt_esa_zarch = 0;
  int i;

  if (argc < 1)
    return NULL;

  arch = strcmp (argv[0], "arch") == 0;
  if (!arch && strcmp (argv[0], "tune"))
    return NULL;
  for (i = 1; i < argc; i++)
    if (strcmp (argv[i], "mesa_mzarch") == 0)
      has_opt_esa_zarch = 1;

  f = fopen ("/proc/cpuinfo", "r");
  if (f == NULL)
    return NULL;

  for (has_features = 0, has_processor = 0;
       (has_features == 0 || has_processor == 0)
	 && fgets (buf, sizeof (buf), f) != NULL; )
    {
      if (has_processor == 0 && strncmp (buf, "processor", 9) == 0)
	{
	  const char *p;
	  long machine_id;

	  p = strstr (buf, "machine = ");
	  if (p == NULL)
	    continue;
	  p += 10;
	  has_processor = 1;
	  machine_id = strtol (p, NULL, 16);
	  switch (machine_id)
	    {
	    case 0x2064:
	    case 0x2066:
	      cpu = "z900";
	      break;
	    case 0x2084:
	    case 0x2086:
	      cpu = "z990";
	      break;
	    case 0x2094:
	    case 0x2096:
	      cpu = "z9-109";
	      is_cpu_z9_109 = 1;
	      break;
	    case 0x2097:
	    case 0x2098:
	      cpu = "z10";
	      break;
	    case 0x2817:
	    case 0x2818:
	      cpu = "z196";
	      break;
	    case 0x2827:
	    case 0x2828:
	      cpu = "zEC12";
	      break;
	    case 0x2964:
	    case 0x2965:
	      cpu = "z13";
	      break;
	    case 0x3906:
	    case 0x3907:
	      cpu = "z14";
	      break;
	    default:
	      cpu = "z14";
	      break;
	    }
	}
      if (has_features == 0 && strncmp (buf, "features", 8) == 0)
	{
	  const char *p;

	  p = strchr (buf, ':');
	  if (p == NULL)
	    continue;
	  p++;
	  while (*p != 0)
	    {
	      int i;

	      while (ISSPACE (*p))
		p++;
	      for (i = 0; !ISSPACE (p[i]) && p[i] != 0; i++)
		;
	      if (i == 3 && strncmp (p, "dfp", 3) == 0)
		has_dfp = 1;
	      else if (i == 2 && strncmp (p, "te", 2) == 0)
		has_te = 1;
	      else if (i == 2 && strncmp (p, "vx", 2) == 0)
		has_vx = 1;
	      else if (i == 8 && strncmp (p, "highgprs", 8) == 0)
		has_highgprs = 1;
	      p += i;
	    }
	  has_features = 1;
	}
    }

  fclose (f);

  if (cpu == NULL)
    return NULL;

  if (arch)
    {
      const char *opt_htm = "";
      const char *opt_vx = "";
      const char *opt_esa_zarch = "";

      /* We may switch off these cpu features but never switch the on
	 explicitly.  This overrides options specified on the command line.  */
      if (!has_te)
	opt_htm = " -mno-htm";
      if (!has_vx)
	opt_vx = " -mno-vx";
      /* However, we set -mzarch only if neither -mzarch nor -mesa are used on
	 the command line.  This allows the user to switch to -mesa manually.
      */
      if (!has_opt_esa_zarch && has_highgprs)
	opt_esa_zarch = " -mzarch";
      options = concat (options, opt_htm, opt_vx, opt_esa_zarch, NULL);
    }
  if (has_dfp && is_cpu_z9_109)
    cpu = "z9-ec";

  return concat ("-m", argv[0], "=", cpu, options, NULL);
}
