/* Configuration for an i386 running GNU with ELF as the target machine.  */

/*
Copyright (C) 1994-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC.  If not, see <http://www.gnu.org/licenses/>.
*/

#define GNU_USER_LINK_EMULATION "elf_i386"

#undef GNU_USER_DYNAMIC_LINKER
#define GNU_USER_DYNAMIC_LINKER "/lib/ld.so"

#ifdef TARGET_LIBC_PROVIDES_SSP

/* i386 glibc provides __stack_chk_guard in %gs:0x14.  */
#define TARGET_THREAD_SSP_OFFSET        0x14

/* We only build the -fsplit-stack support in libgcc if the
   assembler has full support for the CFI directives.  Also
   we only support -fsplit-stack on glibc targets.  */
#if (DEFAULT_LIBC == LIBC_GLIBC) && HAVE_GAS_CFI_PERSONALITY_DIRECTIVE
#define TARGET_CAN_SPLIT_STACK
#endif
/* We steal the last transactional memory word.  */
#define TARGET_THREAD_SPLIT_STACK_OFFSET 0x30
#endif
