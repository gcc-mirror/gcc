/* Copyright (C) 2009, 2010, 2011
   Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

/* List of all known AVR MCU architectures.
   Order as of enum avr_arch from avr.h.  */

const struct base_arch_s
avr_arch_types[] =
{
  /* unknown device specified */
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 32, 1, NULL,              "avr2"  },
  /*
    A  M  J  LM E  E  E         d S   S O  # F
    S  U  M  PO L  L  I         a t   F ff 6 l 
    M  L  P  MV P  P  J  -  -   t a   R s  4 a   
             XW M  M  M         a r     e    s
                   X  P           t     t  k h  */
  { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=1",  "avr1"  },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=2",  "avr2"  },
  { 0, 0, 0, 1, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=25", "avr25" },
  { 0, 0, 1, 0, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=3",  "avr3"  },
  { 0, 0, 1, 0, 1, 0, 0, 0, 0, 0x0060, 32, 2, "__AVR_ARCH__=31", "avr31" },
  { 0, 0, 1, 1, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=35", "avr35" },
  { 0, 1, 0, 1, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=4",  "avr4"  },
  { 0, 1, 1, 1, 0, 0, 0, 0, 0, 0x0060, 32, 1, "__AVR_ARCH__=5",  "avr5"  },
  { 0, 1, 1, 1, 1, 1, 0, 0, 0, 0x0060, 32, 2, "__AVR_ARCH__=51", "avr51" },
  { 0, 1, 1, 1, 1, 1, 1, 0, 0, 0x0060, 32, 4, "__AVR_ARCH__=6",  "avr6"  }
};

const struct mcu_type_s avr_mcu_types[] = {
#define AVR_MCU(NAME, ARCH, MACRO, SHORT_SP, ERRATA_SKIP, DATA_SEC, LIBRARY_NAME) \
  { NAME, ARCH, MACRO, SHORT_SP, ERRATA_SKIP, DATA_SEC, LIBRARY_NAME },
#include "avr-mcus.def"
#undef AVR_MCU
    /* End of list.  */
  { NULL, ARCH_UNKNOWN, NULL, 0, 0, 0, NULL }
};

