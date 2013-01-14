/* Definitions of types that are used to store AVR architecture and
   device information.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay (avr@gjlay.de)

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


/* This enum supplies indices into the avr_arch_types[] table below. */

enum avr_arch
{
  ARCH_UNKNOWN,
  ARCH_AVR1,
  ARCH_AVR2,
  ARCH_AVR25,
  ARCH_AVR3,
  ARCH_AVR31,
  ARCH_AVR35,
  ARCH_AVR4,
  ARCH_AVR5,
  ARCH_AVR51,
  ARCH_AVR6,
  ARCH_AVRXMEGA2,
  ARCH_AVRXMEGA4,
  ARCH_AVRXMEGA5,
  ARCH_AVRXMEGA6,
  ARCH_AVRXMEGA7
};


/* Architecture-specific properties.  */

typedef struct
{
  /* Assembler only.  */
  int asm_only;

  /* Core have 'MUL*' instructions.  */
  int have_mul;

  /* Core have 'CALL' and 'JMP' instructions.  */
  int have_jmp_call;

  /* Core have 'MOVW' and 'LPM Rx,Z' instructions.  */
  int have_movw_lpmx;

  /* Core have 'ELPM' instructions.  */
  int have_elpm;

  /* Core have 'ELPM Rx,Z' instructions.  */
  int have_elpmx;

  /* Core have 'EICALL' and 'EIJMP' instructions.  */
  int have_eijmp_eicall;

  /* This is an XMEGA core.  */
  int xmega_p;

  /* This core has the RAMPD special function register
     and thus also the RAMPX, RAMPY and RAMPZ registers.  */
  int have_rampd;

  /* Default start of data section address for architecture.  */
  int default_data_section_start;

  /* Offset between SFR address and RAM address:
     SFR-address = RAM-address - sfr_offset  */
  int sfr_offset;

  /* Architecture id to built-in define __AVR_ARCH__ (NULL -> no macro) */
  const char *const macro;

  /* Architecture name.  */
  const char *const arch_name;
} avr_arch_t;


/* Device-specific properties.  */

typedef struct
{
  /* Device name.  */
  const char *const name;

  /* Index in avr_arch_types[].  */
  enum avr_arch arch;

  /* Must lie outside user's namespace.  NULL == no macro.  */
  const char *const macro;

  /* Stack pointer have 8 bits width.  */
  int short_sp;

  /* Some AVR devices have a core erratum when skipping a 2-word instruction.
     Skip instructions are:  SBRC, SBRS, SBIC, SBIS, CPSE.
     Problems will occur with return address is IRQ executes during the
     skip sequence.

     A support ticket from Atmel returned the following information:

         Subject: (ATTicket:644469) On AVR skip-bug core Erratum
         From: avr@atmel.com                    Date: 2011-07-27
         (Please keep the subject when replying to this mail)

         This errata exists only in AT90S8515 and ATmega103 devices.

         For information please refer the following respective errata links
            http://www.atmel.com/dyn/resources/prod_documents/doc2494.pdf
            http://www.atmel.com/dyn/resources/prod_documents/doc1436.pdf  */

  /* Core Erratum:  Must not skip 2-word instruction.  */
  int errata_skip;

  /* Start of data section.  */
  int data_section_start;

  /* Number of 64k segments in the flash.  */
  int n_flash;

  /* Name of device library.  */
  const char *const library_name;
} avr_mcu_t;

/* Map architecture to its texinfo string.  */

typedef struct
{
  /* Architecture ID.  */
  enum avr_arch arch;

  /* textinfo source to describe the archtiecture.  */
  const char *texinfo;
} avr_arch_info_t;

/* Preprocessor macros to define depending on MCU type.  */

extern const avr_arch_t avr_arch_types[];
extern const avr_arch_t *avr_current_arch;

extern const avr_mcu_t avr_mcu_types[];
extern const avr_mcu_t *avr_current_device;
