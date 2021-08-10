/* Subroutines for the gcc driver.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

int
loongson_cpucfg (int arg)
{
  int ret;
  __asm__ __volatile__ ("cpucfg %0,%1\n\t"
			:"=r"(ret)
			:"r"(arg)
			:);
  return ret;
}

/* This will be called by the spec parser in gcc.c when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "arch" or "tune" as argument depending on if -march=native
   or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-march=gs464v" on a GS464V for
   -march=native.  If the routine can't detect a known processor,
   the -march or -mtune option is discarded.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */

const char *
host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu = NULL;
  bool arch;
  int cpucfg_arg;
  int cpucfg_ret;

  if (argc < 1)
    return NULL;

  arch = strcmp (argv[0], "arch") == 0;
  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

  cpucfg_arg = 0;
  cpucfg_ret = loongson_cpucfg (cpucfg_arg);
  if (((cpucfg_ret >> 16) & 0xff) == 0x14)
    {
      if (((cpucfg_ret >> 8) & 0xff) == 0xc0)
	cpu = "gs464v";
      else
	cpu = NULL;
    }

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
