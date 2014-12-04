/* Copyright (C) 1998-2014 Free Software Foundation, Inc.
   Contributed by Joern Rennecke

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define IN_GEN_AVR_MMCU_TEXI

#include "avr-arch.h"
#include "avr-devices.c"

static void
print_mcu (const avr_mcu_t *mcu)
{
  const avr_mcu_t *arch_mcu;

  for (arch_mcu = mcu; arch_mcu->macro; )
    arch_mcu--;
  if (arch_mcu->arch != mcu->arch)
    exit (EXIT_FAILURE);

  char name[100];
  if (snprintf (name, sizeof name, "specs-%s", mcu->name) >= sizeof name)
   exit (EXIT_FAILURE);

  FILE *f = fopen (name ,"w");

  const char *sp8, *errata_skip, *rmw;
  /* Leave "avr2" and "avr25" alone.  These two architectures are
     the only ones that mix devices with 8-bit SP and 16-bit SP.  */
  if (mcu->macro == NULL
      && (mcu->arch == ARCH_AVR2 || mcu->arch == ARCH_AVR25))
    sp8 = "";

  sp8 = ((mcu->dev_attribute & AVR_SHORT_SP)
	 ? " -msp8" : " %<msp8");

  errata_skip = (mcu->dev_attribute & AVR_ERRATA_SKIP) ? " -mskip-bug" : "";
  rmw = (mcu->dev_attribute & AVR_ISA_RMW) ? "%{!mno-rmw: -mrmw}" : "";

  const char *arch_name = avr_arch_types[mcu->arch].arch_name;

  fprintf (f, "*self_spec:\n%%{!march=*:-march=%s}%s\n\n", arch_name, sp8);

  if (mcu->macro)
    fprintf (f, "*cpp:\n-D__AVR_DEV_LIB_NAME__=%s -D%s "
	     "-D__AVR_DEVICE_NAME__=%s\n\n",
	     mcu->library_name, mcu->macro, mcu->name);

  fprintf (f, "*cc1:\n%s%s", errata_skip, rmw);
  if (mcu->n_flash != arch_mcu->n_flash)
    fprintf (f, " %%{!mn-flash:-mn-flash=%d}", mcu->n_flash);
  fprintf (f, "\n\n");
  fprintf (f, "*cc1plus:\n%s%s ", errata_skip, rmw);
  if (mcu->n_flash != arch_mcu->n_flash)
    fprintf (f, "%%{!mn-flash:-mn-flash=%d}", mcu->n_flash);
  fprintf (f, "%%{!frtti: -fno-rtti}"
	   "%%{!fenforce-eh-specs: -fno-enforce-eh-specs}"
	   "%%{!fexceptions: -fno-exceptions}\n\n");

  fprintf (f, "*asm:\n%%{march=*:-mmcu=%%*}%{mrelax: --mlink-relax}%s\n\n",
	   *errata_skip ? "" : " -mno-skip-bug");

  fprintf (f, "*link:\n%%{mrelax:--relax");
  if (strncmp (mcu->name, "at90usb8", strlen ("at90usb8")) == 0)
    fprintf (f, "%%{mpmem-wrap-around: --pmem-wrap-around=8k}");
  if (strncmp (mcu->name, "atmega16", strlen ("atmega16")) == 0)
    fprintf (f, "%%{mpmem-wrap-around: --pmem-wrap-around=16k}");
  if (strncmp (mcu->name, "atmega32", strlen ("atmega32")) == 0
      || strncmp (mcu->name, "at90can32", strlen ("at90can32")) == 0)
    fprintf (f, "%%{mpmem-wrap-around: --pmem-wrap-around=32k}");
  if (strncmp (mcu->name, "atmega64", strlen ("atmega64")) == 0
      || strncmp (mcu->name, "at90can64", strlen ("at90can64")) == 0
      || strncmp (mcu->name, "at90usb64", strlen ("at90usb64")) == 0)
    fprintf (f, "%%{mpmem-wrap-around: --pmem-wrap-around=64k}");
  fprintf (f, "} %%{march=*:-m%%*}");
  if (mcu->data_section_start
      != avr_arch_types[mcu->arch].default_data_section_start)
    fprintf (f, " -Tdata 0x%lX", 0x800000UL + mcu->data_section_start);
  if (mcu->text_section_start != 0x0)
    fprintf (f, " -Ttext 0x%lX", mcu->text_section_start);

  fprintf (f, " %%{shared:%%eshared is not supported}\n\n");

  fprintf (f, "*lib:\n");
  if (strncmp (mcu->name, "mmcu=at90s1", strlen ("mmcu=at90s1")) != 0
      && strncmp (mcu->name, "mmcu=attiny11", strlen ("mmcu=attiny11")) != 0
      && strncmp (mcu->name, "mmcu=attiny12", strlen ("mmcu=attiny12")) != 0
      && strncmp (mcu->name, "mmcu=attiny15", strlen ("mmcu=attiny15")) != 0
      && strncmp (mcu->name, "mmcu=attiny28", strlen ("mmcu=attiny28")) != 0)
    {
      fprintf (f, "-lc");
      if (mcu->macro)
	fprintf (f, " dev/%s/libdev.a%%s", mcu->name);
    }
  fprintf (f, "\n\n");

  fprintf (f, "*libgcc:\n");
  if (strncmp (mcu->name, "mmcu=at90s1", strlen ("mmcu=at90s1")) != 0
      && strncmp (mcu->name, "mmcu=attiny11", strlen ("mmcu=attiny11")) != 0
      && strncmp (mcu->name, "mmcu=attiny12", strlen ("mmcu=attiny12")) != 0
      && strncmp (mcu->name, "mmcu=attiny15", strlen ("mmcu=attiny15")) != 0
      && strncmp (mcu->name, "mmcu=attiny28", strlen ("mmcu=attiny28")) != 0)
    fprintf (f, "-lgcc");
  fprintf (f, "\n\n");

  fprintf (f, "*startfile:\ndev/%s/crt1.o%%s\n\n", mcu->name);
}

int main (void)
{
  enum avr_arch arch = ARCH_UNKNOWN;
  size_t i, n_mcus = 0;
  const avr_mcu_t *mcu;

  for (mcu = avr_mcu_types; mcu->name; mcu++)
    print_mcu (mcu);

  return EXIT_SUCCESS;
}
