/* Definitions for ARM EABI ucLinux
   Copyright (C) 2006-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Override settings that are different to the uclinux-elf or
   bpabi defaults.  */

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_SINGLE_PIC_BASE | MASK_INTERWORK)

/* On EABI GNU/Linux, we want both the BPABI builtins and the
   GNU/Linux builtins.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() 		\
  do 						\
    {						\
      TARGET_BPABI_CPP_BUILTINS();		\
      builtin_define ("__uClinux__");		\
      builtin_define ("__gnu_linux__");         \
      builtin_define_std ("linux");             \
      builtin_define_std ("unix");              \
      builtin_assert ("system=linux");          \
      builtin_assert ("system=unix");           \
      builtin_assert ("system=posix");          \
    }						\
  while (false)

#undef SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC " -m armelf_linux_eabi -elf2flt" \
  " --pic-veneer --target2=abs"

/* We default to the "aapcs-linux" ABI so that enums are int-sized by
   default.  */
#undef ARM_DEFAULT_ABI
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS_LINUX

/* Clear the instruction cache from `beg' to `end'.  This makes an
   inline system call to SYS_cacheflush.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(BEG, END)					\
{									\
  register unsigned long _beg __asm ("a1") = (unsigned long) (BEG);	\
  register unsigned long _end __asm ("a2") = (unsigned long) (END);	\
  register unsigned long _flg __asm ("a3") = 0;				\
  register unsigned long _scno __asm ("r7") = 0xf0002;			\
  __asm __volatile ("swi 0x0		@ sys_cacheflush"		\
		    : "=r" (_beg)					\
		    : "0" (_beg), "r" (_end), "r" (_flg), "r" (_scno));	\
}

#define ARM_TARGET2_DWARF_FORMAT DW_EH_PE_absptr
