/* Copyright (C) 1998-2015 Free Software Foundation, Inc.
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

#define GCC_DEFAULTS_H

#include "tm.h"

#if defined (WITH_AVRLIBC)
static const bool with_avrlibc = true;
#else
static const bool with_avrlibc = false;
#endif /* WITH_AVRLIBC */


/* Return true iff STR starts with PREFIX.  */

static bool
str_prefix_p (const char *str, const char *prefix)
{
  return 0 == strncmp (str, prefix, strlen (prefix));
}


static void
print_mcu (const avr_mcu_t *mcu)
{
  const char *sp8_spec;
  const avr_mcu_t *arch_mcu;

  for (arch_mcu = mcu; arch_mcu->macro; )
    arch_mcu--;
  if (arch_mcu->arch != mcu->arch)
    exit (EXIT_FAILURE);

  char name[100];
  if (snprintf (name, sizeof name, "specs-%s", mcu->name) >= (int) sizeof name)
   exit (EXIT_FAILURE);

  FILE *f = fopen (name ,"w");

  bool errata_skip = 0 != (mcu->dev_attribute & AVR_ERRATA_SKIP);
  bool rmw = 0 != (mcu->dev_attribute & AVR_ISA_RMW);
  bool sp8 = 0 != (mcu->dev_attribute & AVR_SHORT_SP);

  if (mcu->macro == NULL
      && (mcu->arch == ARCH_AVR2 || mcu->arch == ARCH_AVR25))
    {
      // Leave "avr2" and "avr25" alone.  These two architectures are
      // the only ones that mix devices with 8-bit SP and 16-bit SP.
      sp8_spec = "";
    }
  else
    {
      sp8_spec = sp8
        ? " -msp8"
        : " %<msp8";
    }

  const char *errata_skip_spec = errata_skip
    ? " %{!mno-skip-bug:-mskip-bug}"
    : " %{!mskip-bug:-mno-skip-bug}";

  const char *rmw_spec = rmw
    ? " %{!mno-rmw: -mrmw}"
    : " %{mrmw}";

  const char *arch_name = avr_arch_types[mcu->arch].arch_name;

  fprintf (f, "*self_spec:\n"
           " %%{!march=*:-march=%s}"
           " %s\n\n", arch_name, sp8_spec);

  if (mcu->macro)
    fprintf (f, "*cpp:\n-D__AVR_DEV_LIB_NAME__=%s -D%s "
	     "-D__AVR_DEVICE_NAME__=%s\n\n",
	     mcu->library_name, mcu->macro, mcu->name);

  fprintf (f, "*cc1:\n%s%s", errata_skip_spec, rmw_spec);
  if (mcu->n_flash != arch_mcu->n_flash)
    fprintf (f, " %%{!mn-flash:-mn-flash=%d}", mcu->n_flash);
  fprintf (f, "\n\n");

  fprintf (f, "*cc1plus:\n%s%s ", errata_skip_spec, rmw_spec);
  if (mcu->n_flash != arch_mcu->n_flash)
    fprintf (f, " %%{!mn-flash:-mn-flash=%d}", mcu->n_flash);
  fprintf (f, (" %%{!frtti: -fno-rtti}"
               " %%{!fenforce-eh-specs: -fno-enforce-eh-specs}"
               " %%{!fexceptions: -fno-exceptions}\n\n"));

  fprintf (f, "*asm:\n"
           " %%{march=*:-mmcu=%%*}"
           " %%{mrelax: --mlink-relax}"
           " %s%s\n\n", rmw_spec, (errata_skip
                                  ? " %{mno-skip-bug}"
                                  : " %{!mskip-bug:-mno-skip-bug}"));
  fprintf (f, "*link:\n"
           " %%{mrelax:--relax");
  {
    int wrap_k =
      str_prefix_p (mcu->name, "at90usb8") ? 8
      : str_prefix_p (mcu->name, "atmega16") ? 16
      : (str_prefix_p (mcu->name, "atmega32")
         || str_prefix_p (mcu->name, "at90can32")) ? 32
      : (str_prefix_p (mcu->name, "atmega64")
        || str_prefix_p (mcu->name, "at90can64")
        || str_prefix_p (mcu->name, "at90usb64")) ? 64
      : 0;

    if (wrap_k)
      fprintf (f, " %%{mpmem-wrap-around: --pmem-wrap-around=%dk}", wrap_k);
  }
  fprintf (f, "}"
           " %%{march=*:-m%%*}");

  if (mcu->data_section_start
      != avr_arch_types[mcu->arch].default_data_section_start)
    fprintf (f, " -Tdata 0x%lX", 0x800000UL + mcu->data_section_start);

  if (mcu->text_section_start != 0x0)
    fprintf (f, " -Ttext 0x%lX", 0UL + mcu->text_section_start);

  fprintf (f, " %%{shared:%%eshared is not supported}\n\n");

  bool has_libs = mcu->arch != ARCH_AVR1;

  fprintf (f, "*lib:\n");
  if (has_libs)
    {
      fprintf (f, "-lc");
      if (with_avrlibc
          && mcu->macro)
	fprintf (f, " dev/%s/libdev.a%%s", mcu->name);
    }
  fprintf (f, "\n\n");

  fprintf (f, "*libgcc:\n");
  if (has_libs)
    fprintf (f, with_avrlibc
             ? "-lgcc -lm"
             : "-lgcc");
  fprintf (f, "\n\n");

  fprintf (f, "*startfile:\n"
           "dev/%s/crt1.o%%s\n\n", mcu->name);
}


int main (void)
{
  for (const avr_mcu_t *mcu = avr_mcu_types; mcu->name; mcu++)
    print_mcu (mcu);

  return EXIT_SUCCESS;
}
