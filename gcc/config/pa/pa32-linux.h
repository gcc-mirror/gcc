/* Definitions for PA_RISC with ELF-32 format
   Copyright (C) 2000-2021 Free Software Foundation, Inc.

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

/* Turn off various SOM crap we don't want.  */
#undef TARGET_ELF32
#define TARGET_ELF32 1

/* The libcall __canonicalize_funcptr_for_compare is referenced in
   crtend.o and the reference isn't resolved in objects that don't
   compare function pointers.  Thus, we need to play games to provide
   a reference in crtbegin.o.  The rest of the define is the same
   as that in crtstuff.c  */
#define CTOR_LIST_BEGIN \
  asm (".type __canonicalize_funcptr_for_compare,@function\n"		\
"	.text\n"							\
"	.word __canonicalize_funcptr_for_compare-$PIC_pcrel$0");	\
  STATIC func_ptr __CTOR_LIST__[1]					\
    __attribute__ ((__used__, section(".ctors"),			\
		    aligned(sizeof(func_ptr))))				\
    = { (func_ptr) (-1) }

/* This is a PIC version of CRT_CALL_STATIC_FUNCTION.  The PIC
   register has to be saved before the call and restored after
   the call.  We assume that register %r4 is available for this
   purpose.  The hack prevents GCC from deleting the restore.  */
#ifdef CRTSTUFFS_O
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
static void __attribute__((__used__))			\
call_ ## FUNC (void)					\
{							\
  asm (SECTION_OP);					\
  asm volatile ("bl " #FUNC ",%%r2\n\t"			\
		"copy %%r19,%%r4\n\t"			\
		"copy %%r4,%%r19\n"			\
		:					\
		:					\
		: "r1", "r2", "r4", "r20", "r21",	\
		  "r22", "r23", "r24", "r25", "r26",	\
		  "r27", "r28", "r29", "r31");		\
  asm (TEXT_SECTION_ASM_OP);				\
}
#endif

/* We need to link against libgcc.a for __canonicalize_funcptr_for_compare
   and $$dyncall.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC GNU_USER_TARGET_ENDFILE_SPEC "libgcc.a%s"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* POSIX types such as pthread_mutex_t require 16-byte alignment to retain
   layout compatibility with the original linux thread implementation.  */
#undef MALLOC_ABI_ALIGNMENT
#define MALLOC_ABI_ALIGNMENT 128

/* Place jump tables in the text section except when generating non-PIC
   code.  When generating non-PIC code, the relocations needed to load the
   address of the jump table result in a text label in the final executable
   if the jump table is placed in the text section.  This breaks the unwind
   data for the function.  Thus, the jump table needs to be placed in
   rodata when generating non-PIC code.  */
#undef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION (flag_pic)

/* We need to override default selection to put references to functions
   in COMDAT groups in .data.rel.ro.local.  */
#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION pa_elf_select_rtx_section
