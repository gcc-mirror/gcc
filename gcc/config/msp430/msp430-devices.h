/* Definitions of subroutines used for reading MCU data on TI MSP430 processors.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by Jozef Lawrynowicz  <jozef.l@mittosystems.com>.

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

struct t_msp430_mcu_data
{
  const char * name;
  unsigned int revision; /* 0=> MSP430, 1=>MSP430X, 2=> MSP430Xv2.  */
  unsigned int hwmpy;    /* 0=>none, 1=>16-bit, 2=>16-bit w/sign extend.  */
			 /* 4=>32-bit, 8=> 32-bit (5xx).  */
};

extern struct t_msp430_mcu_data extracted_mcu_data;

void msp430_extract_mcu_data (const char * mcu_name);
int msp430_check_env_var_for_devices (char **local_devices_csv_loc);
char *msp430_dirname (char *path);
