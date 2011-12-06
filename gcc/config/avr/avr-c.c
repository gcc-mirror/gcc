/* Copyright (C) 2009, 2010
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

/* Not included in avr.c since this requires C front end.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tm_p.h"
#include "cpplib.h"
#include "tree.h"
#include "c-family/c-common.h"
#include "langhooks.h"


/* Implement `REGISTER_TARGET_PRAGMAS'.  */

void
avr_register_target_pragmas (void)
{
  int i;

  gcc_assert (ADDR_SPACE_GENERIC == ADDR_SPACE_RAM);

  /* Register address spaces.  The order must be the same as in the respective
     enum from avr.h (or designated initialized must be used in avr.c).  */

  for (i = 0; avr_addrspace[i].name; i++)
    {
      gcc_assert (i == avr_addrspace[i].id);

      if (!ADDR_SPACE_GENERIC_P (i))
        c_register_addr_space (avr_addrspace[i].name, avr_addrspace[i].id);
    }
}


/* Transorm LO into uppercase and write the result to UP.
   You must provide enough space for UP.  Return UP.  */

static char*
avr_toupper (char *up, const char *lo)
{
  char *up0 = up;
  
  for (; *lo; lo++, up++)
    *up = TOUPPER (*lo);

  *up = '\0';

  return up0;
}
             
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

  if (avr_current_device->errata_skip)
    {
      cpp_define (pfile, "__AVR_ERRATA_SKIP__");
      
      if (avr_current_arch->have_jmp_call)
        cpp_define (pfile, "__AVR_ERRATA_SKIP_JMP_CALL__");
    }

  cpp_define_formatted (pfile, "__AVR_SFR_OFFSET__=0x%x",
                        avr_current_arch->sfr_offset);
    
  /* Define builtin macros so that the user can easily query if or if not
     non-generic address spaces (and which) are supported.
     This is only supported for C.  For C++, a language extension is needed
     (as mentioned in ISO/IEC DTR 18037; Annex F.2) which is not
     implemented in GCC up to now.  */
  
  if (!strcmp (lang_hooks.name, "GNU C"))
    {
      int i;
      
      for (i = 0; avr_addrspace[i].name; i++)
        if (!ADDR_SPACE_GENERIC_P (i))
          {
            const char *name = avr_addrspace[i].name;
            char *Name = (char*) alloca (1 + strlen (name));

            cpp_define_formatted (pfile, "%s=%s",
                                  avr_toupper (Name, name), name);
          }
    }

  /* Define builtin macros so that the user can
     easily query if or if not a specific builtin
     is available. */

  cpp_define (pfile, "__BUILTIN_AVR_NOP");
  cpp_define (pfile, "__BUILTIN_AVR_SEI");
  cpp_define (pfile, "__BUILTIN_AVR_CLI");
  cpp_define (pfile, "__BUILTIN_AVR_WDR");
  cpp_define (pfile, "__BUILTIN_AVR_SLEEP");
  cpp_define (pfile, "__BUILTIN_AVR_SWAP");
  cpp_define (pfile, "__BUILTIN_AVR_MAP8");
  cpp_define (pfile, "__BUILTIN_AVR_MAP16");
  cpp_define (pfile, "__BUILTIN_AVR_DELAY_CYCLES");

  cpp_define (pfile, "__BUILTIN_AVR_FMUL");
  cpp_define (pfile, "__BUILTIN_AVR_FMULS");
  cpp_define (pfile, "__BUILTIN_AVR_FMULSU");

  cpp_define (pfile, "__INT24_MAX__=8388607L");
  cpp_define (pfile, "__INT24_MIN__=(-__INT24_MAX__-1)");
  cpp_define (pfile, "__UINT24_MAX__=16777215UL");
}
