/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1988, 1994, 1995, 1996, 1997, 1999, 2001 Free Software Foundation, Inc.

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

#define TARGET_EXECUTABLE_SUFFIX ".exe"
#define TARGET_OBJECT_SUFFIX ".obj"

/* This enables certain macros in vax.h, which will make an indirect
   reference to an external symbol an invalid address.  This needs to be
   defined before we include vax.h, since it determines which macros
   are used for GO_IF_*.  */

#define NO_EXTERNAL_INDIRECT_ADDRESS

#include "vax/vax.h"

#undef VMS_TARGET
#define VMS_TARGET 1

#undef LIB_SPEC
#undef CPP_PREDEFINES
#undef TARGET_NAME
#undef TARGET_DEFAULT
#undef CALL_USED_REGISTERS
#undef STARTING_FRAME_OFFSET

/* Predefine this in CPP because VMS limits the size of command options
   and GNU CPP is not used on VMS except with GNU C.  */
#define CPP_PREDEFINES \
"-Dvax -Dvms -DVMS -D__vax__ -D__vms__ -D__VMS__\
 -D__GNUC__=2 -D__GNUC_MINOR__=7 -Asystem=vms -Acpu=vax -Amachine=vax"

/* These match the definitions used in VAXCRTL, the VMS C run-time library */

#define SIZE_TYPE	"unsigned int"
#define PTRDIFF_TYPE	"int"
#define WCHAR_TYPE	"unsigned int"
#define WCHAR_TYPE_SIZE	32	/* in bits */

/* Use memcpy for structure copying, and so forth.  */
#define TARGET_MEM_FUNCTIONS

/* Strictly speaking, VMS does not use DBX at all, but the interpreter built
   into gas only speaks straight DBX.  */

#define DEFAULT_GDB_EXTENSIONS 0

#define TARGET_DEFAULT 1
#define TARGET_NAME "vax/vms"

/* The structure return address arrives as an "argument" on VMS.  */
#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE 0
#undef PCC_STATIC_STRUCT_RETURN

#define CALL_USED_REGISTERS {1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

/* The run-time library routine VAXC$ESTABLISH (necessary when mixing
   VMS exception handling and setjmp/longjmp in the same program) requires
   that a hidden automatic variable at the top of the stack be reserved
   for its use.  We accomplish this by simply adding 4 bytes to the local
   stack for all functions, and making sure that normal local variables
   are 4 bytes lower on the stack then they would otherwise have been.  */

#define STARTING_FRAME_OFFSET -4

/* This macro definition sets up a default value for `main' to return.  */
#define DEFAULT_MAIN_RETURN  c_expand_return (integer_one_node)

/* This makes use of a hook in varasm.c to mark all external variables
   for us.  We use this to make sure that external variables are correctly
   addressed.  Under VMS there is some brain damage in the linker that requires
   us to do this.  */

#define ENCODE_SECTION_INFO(decl)  				\
  if (DECL_EXTERNAL (decl) && TREE_PUBLIC (decl)) 		\
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1; 

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#undef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(FILE,NAME)		\
  do { fputs (".globl ", FILE);			\
       assemble_name (FILE, NAME);		\
       fputs ("\n", FILE);			\
       vms_check_external (NULL_TREE, NAME, 0); \
     } while (0)

/* Under VMS we write the actual size of the storage to be allocated even
   though the symbol is external.  Although it is possible to give external
   symbols a size of 0 (as unix does), the VMS linker does not make the
   distinction between a variable definition and an external reference of a
   variable, and thus the linker will not complain about a missing definition.
   If we followed the unix example of giving external symbols a size of
   zero, you tried to link a program where a given variable was externally
   defined but none of the object modules contained a non-extern definition,
   the linker would allocate 0 bytes for the variable, and any attempt to
   use that variable would use the storage allocated to some other variable.

   We must also select either const_section or data_section: this will indicate
   whether or not the variable will get the readonly bit set.  Since the
   VMS linker does not distinguish between a variable's definition and an
   external reference, all usages of a given variable must have the readonly
   bit set the same way, or the linker will get confused and give warning
   messages.  */

/* We used to round the size up to a multiple of 4,
   but that causes linker errors sometimes when the variable was initialized
   since the size of its definition was not likewise rounded up.  */

/* Note:  the original ASM_OUTPUT_EXTERNAL code has been moved into
   vms_check_external and vms_flush_pending_externals.  */

#define ASM_OUTPUT_EXTERNAL(FILE,DECL,NAME)				\
{ if (DECL_INITIAL (DECL) == 0 && TREE_CODE (DECL) != FUNCTION_DECL)	\
    vms_check_external ((DECL), (NAME), 1);				\
}

/* ASM_OUTPUT_EXTERNAL will have wait until after an initializer is
   completed in order to switch sections for an external object, so
   use the DECLARE_OBJECT hooks to manage deferred declarations.  */

/* This is the default action for ASM_DECLARE_OBJECT_NAME, but if it
   is explicitly defined, then ASM_FINISH_DECLARE_OBJECT will be used.  */

#define ASM_DECLARE_OBJECT_NAME(ASM_OUT_FILE,NAME,DECL)		\
  ASM_OUTPUT_LABEL ((ASM_OUT_FILE), (NAME))

/* We don't need to do anything special to finish the current object, but it
   should now be safe to output any deferred external global declarations.  */

#define ASM_FINISH_DECLARE_OBJECT(FILE,DECL,TOPLVL,ATEND)		\
  vms_flush_pending_externals(FILE)

/* Anything still pending must be flushed at the very end.  */

#define ASM_FILE_END(STREAM)						\
  vms_flush_pending_externals(STREAM)

/* Here we redefine ASM_OUTPUT_COMMON to select the data_section or the
   const_section before writing the ".const" assembler directive.
   If we were specifying a size of zero for external variables, we would
   not have to select a section, since the assembler can assume that
   when the size > 0, the storage is for a non-external, uninitialized
   variable (for which a "const" declaration would be senseless),
   and the assembler can make the storage read/write.

   Since the ".const" directive specifies the actual size of the storage used
   for both external and non-external variables, the assembler cannot
   make this assumption, and thus it has no way of deciding if storage should
   be read/write or read-only.  To resolve this, we give the assembler some
   assistance, in the form of a ".const" or a ".data" directive.

   Under GCC 1.40, external variables were declared with a size of zero.
   The GNU assembler, GAS, will recognize the "-2" switch when built for VMS;
   when compiling programs with GCC 2.n this switch should be used or the
   assembler will not give the read-only attribute to external constants.
   Failure to use this switch will result in linker warning messages about
   mismatched psect attributes.  */

#undef ASM_OUTPUT_COMMON

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)		\
( ((TREE_READONLY (decl) && ! TREE_THIS_VOLATILE (decl))	\
   ? (const_section (), 0) : (data_section (), 0)),		\
  fputs (".comm ", (FILE)),					\
  assemble_name ((FILE), (NAME)),				\
  fprintf ((FILE), ",%u\n", (SIZE)))

/* We define this to prevent the name mangler from putting dollar signs into
   function names.  This isn't really needed, but it has been here for
   some time and  removing it would cause the object files generated by the
   compiler to be incompatible with the object files from a compiler that
   had this defined.  Since it does no harm, we leave it in.  */

#define NO_DOLLAR_IN_LABEL

/* Add a "const" section.  This is viewed by the assembler as being nearly
   the same as the "data" section, with the only difference being that a
   flag is set for variables declared while in the const section.  This
   flag is used to determine whether or not the read/write bit should be
   set in the Psect definition.  */

#define EXTRA_SECTIONS in_const

#define EXTRA_SECTION_FUNCTIONS				\
void							\
const_section ()					\
{							\
  if (in_section != in_const) {				\
    fprintf(asm_out_file,".const\n");			\
    in_section = in_const;				\
  }							\
}

/* This macro contains the logic to decide which section a variable
   should be stored in.  Static constant variables go in the text_section,
   non-const variables go in the data_section, and non-static const
   variables go in the const_section.

   Since this macro is used in a number of places, we must also be able
   to decide where to place string constants.  */

#define SELECT_SECTION(T,RELOC,ALIGN)					\
{									\
  if (TREE_CODE (T) == VAR_DECL)					\
    {									\
      if (TREE_READONLY (T) && ! TREE_THIS_VOLATILE (T)			\
	  && DECL_INITIAL (T)						\
	  && (DECL_INITIAL (T) == error_mark_node			\
	      || TREE_CONSTANT (DECL_INITIAL (T))))			\
	{								\
	  if (TREE_PUBLIC (T))						\
	    const_section ();						\
	  else								\
	    text_section ();						\
	}								\
      else								\
	data_section ();						\
    }									\
  if (TREE_CODE_CLASS (TREE_CODE (T)) == 'c')				\
    {									\
      if ((TREE_CODE (T) == STRING_CST && flag_writable_strings))	\
	data_section ();						\
      else								\
	text_section ();						\
    }									\
}

/* This is used by a hook in varasm.c to write the assembler directives
   that are needed to tell the startup code which constructors need to
   be run.  */

#define TARGET_ASM_CONSTRUCTOR  vms_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR   vms_asm_out_destructor

/* The following definitions are used in libgcc2.c with the __main
   function.  The _SHR symbol is used when the sharable image library
   for the C++ library is used - this is picked up automatically by the linker
   and this symbol points to the start of __CTOR_LIST__ from the C++ library.
   If the C++ library is not used, then __CTOR_LIST_SHR__ occurs just after
   __CTOR_LIST__, and essentially points to the same list as __CTOR_LIST.  */

#ifdef L__main

#define __CTOR_LIST__ __gxx_init_0
#define __CTOR_LIST_END__ __gxx_init_2

#define __CTOR_LIST_SHR__ $$PsectAttributes_NOSHR$$__gxx_init_0_shr
#define __CTOR_LIST_SHR_END__ $$PsectAttributes_NOSHR$$__gxx_init_2_shr

#define DO_GLOBAL_CTORS_BODY						\
do {									\
  func_ptr *p;								\
  extern func_ptr __CTOR_LIST__[1], __CTOR_LIST_END__[1];		\
  extern func_ptr __CTOR_LIST_SHR__[1], __CTOR_LIST_SHR_END__[1];	\
  if (&__CTOR_LIST_SHR__[0] != &__CTOR_LIST__[1])			\
    for (p = __CTOR_LIST_SHR__ + 1; p < __CTOR_LIST_SHR_END__ ; p++ )	\
      if (*p) (*p) ();							\
  for (p = __CTOR_LIST__ + 1; p < __CTOR_LIST_END__ ; p++ )		\
    if (*p) (*p) ();							\
  do {	/* arrange for `return' from main() to pass through exit() */	\
      __label__ foo;							\
      int *callers_caller_fp = (int *) __builtin_frame_address (3);	\
      register int retval asm ("r0");					\
      callers_caller_fp[4] = (int) && foo;				\
      break;		/* out of do-while block */			\
    foo:								\
      exit (retval);							\
  } while (0);								\
} while (0)

#define __DTOR_LIST__ __gxx_clean_0
#define __DTOR_LIST_END__ __gxx_clean_2

#define __DTOR_LIST_SHR__ $$PsectAttributes_NOSHR$$__gxx_clean_0_shr
#define __DTOR_LIST_SHR_END__ $$PsectAttributes_NOSHR$$__gxx_clean_2_shr

#define DO_GLOBAL_DTORS_BODY						\
do {									\
  func_ptr *p;								\
  extern func_ptr __DTOR_LIST__[1], __DTOR_LIST_END__[1];		\
  extern func_ptr __DTOR_LIST_SHR__[1], __DTOR_LIST_SHR_END__[1];	\
  for (p = __DTOR_LIST__ + 1; p < __DTOR_LIST_END__ ; p++ )		\
    if (*p) (*p) ();							\
  if (&__DTOR_LIST_SHR__[0] != &__DTOR_LIST__[1])			\
    for (p = __DTOR_LIST_SHR__ + 1; p < __DTOR_LIST_SHR_END__ ; p++ )	\
      if (*p) (*p) ();							\
} while (0)

#endif /* L__main */

/* Specify the list of include file directories.  */
#define INCLUDE_DEFAULTS \
{									\
  { "GNU_GXX_INCLUDE:", "G++", 1, 1 },					\
  { "GNU_CC_INCLUDE:", "GCC", 0, 0 },	/* GNU includes */		\
  { "SYS$SYSROOT:[SYSLIB.]", 0, 0, 0 }, /* VAX-11 "C" includes */	\
  { ".", 0, 0, 1 },		/* Make normal VMS filespecs work.  */	\
  { 0, 0, 0, 0 }							\
}
