/* Copyright (C) 2009
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
#include "tm_p.h"
#include "regs.h"
#include "c-common.h"


/* Not included in avr.c since this requires C front end.  */

/* Worker function for TARGET_CPU_CPP_BUILTINS.  */

void
avr_cpu_cpp_builtins (struct cpp_reader *pfile)
{
  builtin_define_std ("AVR");

  if (avr_current_arch->macro)
    cpp_define (pfile, avr_current_arch->macro);
  if (avr_extra_arch_macro)
    cpp_define (pfile, avr_extra_arch_macro);
  if (avr_current_arch->have_elpm)
    cpp_define (pfile, "__AVR_HAVE_RAMPZ__");
  if (avr_current_arch->have_elpm)
    cpp_define (pfile, "__AVR_HAVE_ELPM__");
  if (avr_current_arch->have_elpmx)
    cpp_define (pfile, "__AVR_HAVE_ELPMX__");
  if (avr_current_arch->have_movw_lpmx)
    {
      cpp_define (pfile, "__AVR_HAVE_MOVW__");
      cpp_define (pfile, "__AVR_HAVE_LPMX__");
    }
  if (avr_current_arch->asm_only)
    cpp_define (pfile, "__AVR_ASM_ONLY__");
  if (avr_current_arch->have_mul)
    {
      cpp_define (pfile, "__AVR_ENHANCED__");
      cpp_define (pfile, "__AVR_HAVE_MUL__");
    }
  if (avr_current_arch->have_jmp_call)
    {
      cpp_define (pfile, "__AVR_MEGA__");
      cpp_define (pfile, "__AVR_HAVE_JMP_CALL__");
    }
  if (avr_current_arch->have_eijmp_eicall)
    {
      cpp_define (pfile, "__AVR_HAVE_EIJMP_EICALL__");
      cpp_define (pfile, "__AVR_3_BYTE_PC__");
    }
  else
    {
      cpp_define (pfile, "__AVR_2_BYTE_PC__");
    }

  if (avr_current_device->short_sp)
    cpp_define (pfile, "__AVR_HAVE_8BIT_SP__");
  else
    cpp_define (pfile, "__AVR_HAVE_16BIT_SP__");

  if (TARGET_NO_INTERRUPTS)
    cpp_define (pfile, "__NO_INTERRUPTS__");
}

