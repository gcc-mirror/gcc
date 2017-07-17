/* Copyright (C) 2009-2017 Free Software Foundation, Inc.
   Contributed by Anatoly Sokolov (aesok@post.ru)

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

#ifndef IN_GEN_AVR_MMCU_TEXI
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic.h"
#endif /* IN_GEN_AVR_MMCU_TEXI */

#include "avr-arch.h"

/* List of all known AVR MCU architectures.
   Order as of enum avr_arch from avr.h.  */

const avr_arch_t
avr_arch_types[] =
{
  /* unknown device specified */
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, NULL, AVR_MMCU_DEFAULT },
  /*
    A  M  J  LM E  E  E  X  R  T  d S     FPO     S O   A
    S  U  M  PO L  L  I  M  A  I  a t     lMff    F ff  r
    M  L  P  MV P  P  J  E  M  N  t a     a s     R s   c
             XW M  M  M  G  P  Y  a r     s e       e   h
                   X  P  A  D       t     h t       t   ID   */
  { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "1",   "avr1"  },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "2",   "avr2"  },
  { 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "25",  "avr25" },
  { 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "3",   "avr3"  },
  { 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0x0060, 0,      32, "31",  "avr31" },
  { 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "35",  "avr35" },
  { 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "4",   "avr4"  },
  { 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0x0060, 0,      32, "5",   "avr5"  },
  { 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0x0060, 0,      32, "51",  "avr51" },
  { 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0x0060, 0,      32, "6",   "avr6"  },

  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0x0040, 0x4000, 0, "100", "avrtiny" },
  { 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0x2000, 0,      0, "102", "avrxmega2" },
  { 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0x2000, 0x8000, 0, "103", "avrxmega3" },
  { 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0x2000, 0,      0, "104", "avrxmega4" },
  { 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0x2000, 0,      0, "105", "avrxmega5" },
  { 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0x2000, 0,      0, "106", "avrxmega6" },
  { 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0x2000, 0,      0, "107", "avrxmega7" }
};

const avr_arch_info_t
avr_texinfo[] =
{
  { ARCH_AVR1,
    "This ISA is implemented by the minimal AVR core and supported "
    "for assembler only." },
  { ARCH_AVR2,
    "``Classic'' devices with up to 8@tie{}KiB of program memory." },
  { ARCH_AVR25,
    "``Classic'' devices with up to 8@tie{}KiB of program memory and with "
    "the @code{MOVW} instruction." },
  { ARCH_AVR3,
    "``Classic'' devices with 16@tie{}KiB up to 64@tie{}KiB of "
    " program memory." },
  { ARCH_AVR31,
    "``Classic'' devices with 128@tie{}KiB of program memory." },
  { ARCH_AVR35,
    "``Classic'' devices with 16@tie{}KiB up to 64@tie{}KiB of "
    "program memory and with the @code{MOVW} instruction." },
  { ARCH_AVR4,
    "``Enhanced'' devices with up to 8@tie{}KiB of program memory." },
  { ARCH_AVR5,
    "``Enhanced'' devices with 16@tie{}KiB up to 64@tie{}KiB of "
    "program memory." },
  { ARCH_AVR51,
    "``Enhanced'' devices with 128@tie{}KiB of program memory." },
  { ARCH_AVR6,
    "``Enhanced'' devices with 3-byte PC, i.e.@: with more than 128@tie{}KiB "
    "of program memory." },
  { ARCH_AVRTINY,
    "``TINY'' Tiny core devices with 512@tie{}B up to 4@tie{}KiB of "
    "program memory." },
  { ARCH_AVRXMEGA2,
    "``XMEGA'' devices with more than 8@tie{}KiB and up to 64@tie{}KiB "
    "of program memory." },
  { ARCH_AVRXMEGA3,
    "``XMEGA'' devices with up to 64@tie{}KiB of combined program memory "
    "and RAM, and with program memory visible in the RAM address space." },
  { ARCH_AVRXMEGA4,
    "``XMEGA'' devices with more than 64@tie{}KiB and up to 128@tie{}KiB "
    "of program memory." },
  { ARCH_AVRXMEGA5,
    "``XMEGA'' devices with more than 64@tie{}KiB and up to 128@tie{}KiB "
    "of program memory and more than 64@tie{}KiB of RAM." },
  { ARCH_AVRXMEGA6,
    "``XMEGA'' devices with more than 128@tie{}KiB of program memory." },
  { ARCH_AVRXMEGA7,
    "``XMEGA'' devices with more than 128@tie{}KiB of program memory "
    "and more than 64@tie{}KiB of RAM." }
};

const avr_mcu_t
avr_mcu_types[] =
{
#define AVR_MCU(NAME, ARCH, DEV_ATTRIBUTE, MACRO, DATA_SEC, TEXT_SEC, FLASH_SIZE)\
  { NAME, ARCH, DEV_ATTRIBUTE, MACRO, DATA_SEC, TEXT_SEC, FLASH_SIZE },
#include "avr-mcus.def"
#undef AVR_MCU
    /* End of list.  */
  { NULL, ARCH_UNKNOWN, AVR_ISA_NONE, NULL, 0, 0, 0 }
};




#ifndef IN_GEN_AVR_MMCU_TEXI

static char*
avr_archs_str (void)
{
  char *archs = concat ("", NULL);

  // Build of core architectures' names.

  for (const avr_mcu_t *mcu = avr_mcu_types; mcu->name; mcu++)
    if (!mcu->macro)
      archs = concat (archs, " ", avr_arch_types[mcu->arch_id].name, NULL);

  return archs;
}

  
void
avr_inform_core_architectures (void)
{
  char *archs = avr_archs_str ();
  inform (input_location, "supported core architectures:%s", archs);
  free (archs);
}

#endif // IN_GEN_AVR_MMCU_TEXI
