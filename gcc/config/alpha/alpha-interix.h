/* Definitions of target machine for GNU compiler, for DEC Alpha
   running Windows/NT.
   Copyright (C) 1995, 1996, 1999, 2000, 2002 Free Software Foundation, Inc.

   Donn Terry, Softway Systems, Inc.
   From code
       Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* cpp handles __STDC__ */
/* The three "Alpha" defines on the first such line are from the CLAXP spec */
#define TARGET_OS_CPP_BUILTINS()				\
    do {							\
	builtin_define ("__INTERIX");				\
	builtin_define ("__OPENNT");				\
	builtin_define ("__Alpha_AXP");				\
	builtin_define ("_M_ALPHA");				\
	builtin_define ("_ALPHA_");				\
	builtin_define ("__stdcall=");				\
	builtin_define ("__cdecl=");				\
	builtin_assert ("system=unix");				\
	builtin_assert ("system=interix");			\
    } while (0)

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
-remap \
%{posix:-D_POSIX_SOURCE} \
-isystem %$INTERIX_ROOT/usr/include"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (alpha Interix)");

/* alpha.h sets this, but it doesn't apply to us */
#undef OBJECT_FORMAT_ECOFF
#undef OBJECT_FORMAT_COFF

/* LINK_SPEC */

/* MD_STARTFILE_PREFIX */

/* ASM_OUTPUT_LOOP_ALIGN; ASM_OUTPUT_ALIGN_CODE */

/* Codegen macro overrides for NT internal conventions */

/* the below are ecoff specific... we don't need them, so
   undef them (they'll get a default later) */

#undef PUT_SDB_BLOCK_START
#undef PUT_SDB_BLOCK_END

/* The following are needed for C++, but also needed for profiling */

/* Support const sections and the ctors and dtors sections for g++.  */

#define READONLY_DATA_SECTION_ASM_OP	"\t.rdata"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.

   Note that we want to give these sections the SHF_WRITE attribute
   because these sections will actually contain data (i.e. tables of
   addresses of functions in the current root executable or shared library
   file) and, in the case of a shared library, the relocatable addresses
   will have to be properly resolved/relocated (and then written into) by
   the dynamic linker when it actually attaches the given shared library
   to the executing process.  (Note that on SVR4, you may wish to use the
   `-z text' option to the ELF linker, when building a shared library, as
   an additional check that you are doing everything right.  But if you do
   use the `-z text' option when building a shared library, you will get
   errors unless the .ctors and .dtors sections are marked as writable
   via the SHF_WRITE attribute.)  */

#define CTORS_SECTION_ASM_OP	"\t.ctors"
#define DTORS_SECTION_ASM_OP	"\t.dtors"

/* The linker will take care of this, and having them causes problems with
   ld -r (specifically -rU).  */
#define CTOR_LISTS_DEFINED_EXTERNALLY 1

#define SET_ASM_OP	"\t.set\t"
/* Output a definition (implements alias) */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
do									\
{									\
    fprintf ((FILE), "\t");						\
    assemble_name (FILE, LABEL1);					\
    fprintf (FILE, "=");						\
    assemble_name (FILE, LABEL2);					\
    fprintf (FILE, "\n");						\
    }									\
while (0)

/* We use the defaults, so undef the null definitions */
#undef PUT_SDB_FUNCTION_START
#undef PUT_SDB_FUNCTION_END
#undef PUT_SDB_EPILOGUE_END

#define HOST_PTR_PRINTF "%p"
#define HOST_PTR_AS_INT unsigned long

#define PCC_BITFIELD_TYPE_MATTERS 1
#define PCC_BITFIELD_TYPE_TEST TYPE_NATIVE(rec)
#define GROUP_BITFIELDS_BY_ALIGN TYPE_NATIVE(rec)

/* DWARF2 Unwinding doesn't work with exception handling yet.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.

   On NT (according to the spec) anything except strings/array that fits
   in 64 bits is returned in the registers (this appears to differ from
   the rest of the Alpha family).  */

#undef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE) \
  (TREE_CODE (TYPE) == ARRAY_TYPE || int_size_in_bytes(TYPE) > 8)

#define ASM_LOAD_ADDR(loc, reg)   "     lda " #reg "," #loc "\n" 

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");                          \
  fprintf (FILE, "\t.set noat\n");				\
  fprintf (FILE, "\t.globl\t__fltused\n");			\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}
