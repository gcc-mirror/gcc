/* Subroutines for the gcc driver.
   Copyright (C) 2008, 2011 Free Software Foundation, Inc.

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

#ifdef __sgi__
#include <invent.h>
#include <sys/sbd.h>

/* Cf. MIPS R10000 Microprocessor User Guide, Version 2.0, 14.13 Processor
   Revision Identifier (PRId) Register (15).

   http://techpubs.sgi.com/library/tpl/cgi-bin/getdoc.cgi/hdwr/bks/SGI_Developer/books/R10K_UM/sgi_html/t5.Ver.2.0.book_279.html  */

static const struct cpu_types {
  int impl;
  const char *cpu;
} cpu_types[] = {
  { C0_IMP_R2000, "r2000" },
  { C0_IMP_R3000, "r3000" },
  { C0_IMP_R6000, "r6000" },
  { C0_IMP_R4000, "r4000" },
  { C0_IMP_R6000A, "r6000" },
  { C0_IMP_R10000, "r10000" },
  { C0_IMP_R12000, "r12000" },
  { C0_IMP_R14000, "r14000" },
  { C0_IMP_R8000,  "r8000" },
  { C0_IMP_R4600,  "r4600" },
  { C0_IMP_R4700,  "r4600" },
  { C0_IMP_R4650,  "r4650" },
  { C0_IMP_R5000,  "vr5000" },
  { C0_IMP_RM7000, "rm7000" },
  { C0_IMP_RM5271, "vr5000" },
  { 0, 0 }
};

static int
cputype (inventory_t *inv, void *arg)
{
  if (inv != NULL
      && inv->inv_class == INV_PROCESSOR
      && inv->inv_type == INV_CPUCHIP)
    {
      int i;
      /* inv_state is the cpu revision number.  */
      int impl = (inv->inv_state & C0_IMPMASK) >> C0_IMPSHIFT;

      for (i = 0; cpu_types[i].cpu != NULL; i++)
	if (cpu_types[i].impl == impl)
	  {
	    *((const char **) arg) = cpu_types[i].cpu;
	    break;
	  }
    }
  return 0;
}
#endif

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
#ifndef __sgi__
  char buf[128];
  FILE *f;
#endif
  bool arch;

  if (argc < 1)
    return NULL;

  arch = strcmp (argv[0], "arch") == 0;
  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

#ifdef __sgi__
  scaninvent (cputype, &cpu);
#else
  f = fopen ("/proc/cpuinfo", "r");
  if (f == NULL)
    return NULL;

  while (fgets (buf, sizeof (buf), f) != NULL)
    if (strncmp (buf, "cpu model", sizeof ("cpu model") - 1) == 0)
      {
	if (strstr (buf, "Godson2 V0.2") != NULL
	    || strstr (buf, "Loongson-2 V0.2") != NULL)
	  cpu = "loongson2e";
	else if (strstr (buf, "Godson2 V0.3") != NULL
		 || strstr (buf, "Loongson-2 V0.3") != NULL)
	  cpu = "loongson2f";
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
#endif

  if (cpu == NULL)
    return NULL;

  return concat ("-m", argv[0], "=", cpu, NULL);
}
