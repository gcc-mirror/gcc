/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using a Unix style C library and tools.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

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

#define YES_UNDERSCORES

#define DBX_DEBUGGING_INFO 
#define SDB_DEBUGGING_INFO 
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define TARGET_EXECUTABLE_SUFFIX ".exe"

#include <stdio.h>
#include "i386/gas.h"
#include "dbxcoff.h"

/* Augment TARGET_SWITCHES with the cygwin/no-cygwin options.  */
#define MASK_WIN32 0x40000000 /* Use -lming32 interface */
#define MASK_CYGWIN  0x20000000 /* Use -lcygwin interface */
#define MASK_WINDOWS 0x10000000 /* Use windows interface */
#define MASK_DLL     0x08000000 /* Use dll interface    */
#define MASK_NOP_FUN_DLLIMPORT 0x20000 /* Ignore dllimport for functions */

#define TARGET_WIN32             (target_flags & MASK_WIN32)
#define TARGET_CYGWIN            (target_flags & MASK_CYGWIN)
#define TARGET_WINDOWS           (target_flags & MASK_WINDOWS)
#define TARGET_DLL               (target_flags & MASK_DLL)
#define TARGET_NOP_FUN_DLLIMPORT (target_flags & MASK_NOP_FUN_DLLIMPORT)

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
{ "cygwin",		  MASK_CYGWIN,					\
  N_("Use the Cygwin interface") },					\
{ "no-cygwin",		  MASK_WIN32,					\
  N_("Use the Mingw32 interface") },					\
{ "windows",		  MASK_WINDOWS, N_("Create GUI application") },	\
{ "no-win32",		  -MASK_WIN32, N_("Don't set Windows defines") },\
{ "win32",		  0, N_("Set Windows defines") },		\
{ "console",		  -MASK_WINDOWS,				\
  N_("Create console application") }, 					\
{ "dll",		  MASK_DLL, N_("Generate code for a DLL") },	\
{ "nop-fun-dllimport",	  MASK_NOP_FUN_DLLIMPORT,			\
  N_("Ignore dllimport for functions") }, 				\
{ "no-nop-fun-dllimport", -MASK_NOP_FUN_DLLIMPORT, "" }, \
{ "threads",		  0, N_("Use Mingw-specific thread support") },

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_X86_=1 -Asystem=winnt"

#ifdef CROSS_COMPILE
#define CYGWIN_INCLUDES "%{!nostdinc:-idirafter " CYGWIN_CROSS_DIR "/include}"
#define W32API_INC "%{!nostdinc:-idirafter " CYGWIN_CROSS_DIR "/include/w32api}"
#define W32API_LIB "-L" CYGWIN_CROSS_DIR "/lib/w32api/"
#define CYGWIN_LIB CYGWIN_CROSS_DIR "/lib"
#define MINGW_LIBS "-L" CYGWIN_CROSS_DIR "/lib/mingw"
#define MINGW_INCLUDES "%{!nostdinc:-isystem " CYGWIN_CROSS_DIR "/include/mingw/g++-3 "\
		       "-isystem " CYGWIN_CROSS_DIR "/include/mingw/g++ "\
		       "-idirafter " CYGWIN_CROSS_DIR "/include/mingw}"
#else
#define CYGWIN_INCLUDES "%{!nostdinc:-isystem /usr/local/include "\
		           "-idirafter " CYGWIN_CROSS_DIR "/include "\
		           "-idirafter /usr/include}"
#define W32API_INC "%{!nostdinc:"\
		      "-idirafter " CYGWIN_CROSS_DIR "/include/w32api "\
		      "-idirafter /usr/include/w32api}"
#define W32API_LIB "-L" CYGWIN_CROSS_DIR "/lib/w32api/ -L/usr/lib/w32api/"
#define CYGWIN_LIB "/usr/lib"
#define MINGW_LIBS "-L/usr/local/lib/mingw -L/usr/lib/mingw"
#define MINGW_INCLUDES "%{!nostdinc:-isystem /usr/include/mingw/g++-3 "\
		       "-isystem /usr/include/mingw/g++ "\
		       "-isystem /usr/local/include/mingw "\
		       "-idirafter " CYGWIN_CROSS_DIR "/include/mingw "\
		       "-idirafter /usr/include/mingw}"
#endif

/* Get tree.c to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES

/* Support the __declspec keyword by turning them into attributes.
   We currently only support: dllimport and dllexport.
   Note that the current way we do this may result in a collision with
   predefined attributes later on.  This can be solved by using one attribute,
   say __declspec__, and passing args to it.  The problem with that approach
   is that args are not accumulated: each new appearance would clobber any
   existing args.  */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{posix:-D_POSIX_SOURCE} \
  -D__stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  %{!ansi:-D_stdcall=__attribute__((__stdcall__)) \
    -D_cdecl=__attribute__((__cdecl__))} \
  -D__declspec(x)=__attribute__((x)) \
  -D__i386__ -D__i386 \
  %{mno-win32:%{mno-cygwin: %emno-cygwin and mno-win32 are not compatible}} \
  %{mno-cygwin:-D__MSVCRT__ -D__MINGW32__ %{mthreads:-D_MT} "\
    MINGW_INCLUDES "} \
  %{!mno-cygwin:-D__CYGWIN32__ -D__CYGWIN__ %{!ansi:-Dunix} -D__unix__ -D__unix "\
    CYGWIN_INCLUDES "}\
  %{mwin32|mno-cygwin:-DWIN32 -D_WIN32 -D__WIN32 -D__WIN32__ %{!ansi:-DWINNT}}\
  %{!mno-win32:" W32API_INC "}\
"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
  %{shared|mdll: %{mno-cygwin:" MINGW_LIBS " dllcrt2%O%s}}\
  %{!shared: %{!mdll: %{!mno-cygwin:crt0%O%s} %{mno-cygwin:" MINGW_LIBS " crt2%O%s}\
  %{pg:gcrt0%O%s}}}\
"

/* Normally, -lgcc is not needed since everything in it is in the DLL, but we
   want to allow things to be added to it when installing new versions of
   GCC without making a new CYGWIN.DLL, so we leave it.  Profiling is handled
   by calling the init function from the prologue.  */

#undef LIBGCC_SPEC
#define LIBGCC_SPEC "%{mno-cygwin: %{mthreads:-lmingwthrd} -lmingw32} -lgcc %{mno-cygwin:-lmoldname -lmsvcrt}"

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS 						\
  { "mingw_include_path", DEFAULT_TARGET_MACHINE }

/* We have to dynamic link to get to the system DLLs.  All of libc, libm and
   the Unix stuff is in cygwin.dll.  The import library is called
   'libcygwin.a'.  For Windows applications, include more libraries, but
   always include kernel32.  We'd like to specific subsystem windows to
   ld, but that doesn't work just yet.  */

#undef LIB_SPEC
#define LIB_SPEC "\
  %{pg:-lgmon} \
  %{!mno-cygwin:-lcygwin} \
  %{mno-cygwin:%{mthreads:-lmingwthrd} -lmingw32} \
  %{mwindows:-lgdi32 -lcomdlg32} \
  -luser32 -lkernel32 -ladvapi32 -lshell32"

#define LINK_SPEC W32API_LIB "\
  %{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: -e \
    %{mno-cygwin:_DllMainCRTStartup@12} \
    %{!mno-cygwin:__cygwin_dll_entry@12}}\
  --dll-search-prefix=cyg"

#undef MATH_LIBRARY
#define MATH_LIBRARY ""

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"


/* Enable parsing of #pragma pack(push,<n>) and #pragma pack(pop).  */
#define HANDLE_PRAGMA_PACK_PUSH_POP 1

union tree_node;
#define TREE union tree_node *

/* Used to implement dllexport overriding dllimport semantics.  It's also used
   to handle vtables - the first pass won't do anything because
   DECL_CONTEXT (DECL) will be 0 so i386_pe_dll{ex,im}port_p will return 0.
   It's also used to handle dllimport override semantics.  */
#if 0
#define REDO_SECTION_INFO_P(DECL) \
  ((DECL_ATTRIBUTES (DECL) != NULL_TREE) \
   || (TREE_CODE (DECL) == VAR_DECL && DECL_VIRTUAL_P (DECL)))
#else
#define REDO_SECTION_INFO_P(DECL) 1
#endif


#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_drectve

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  DRECTVE_SECTION_FUNCTION					\
  SWITCH_TO_SECTION_FUNCTION

#define DRECTVE_SECTION_FUNCTION \
void									\
drectve_section ()							\
{									\
  if (in_section != in_drectve)						\
    {									\
      fprintf (asm_out_file, "%s\n", "\t.section .drectve\n");		\
      in_section = in_drectve;						\
    }									\
}
void drectve_section PARAMS ((void));

/* Switch to SECTION (an `enum in_section').

   ??? This facility should be provided by GCC proper.
   The problem is that we want to temporarily switch sections in
   ASM_DECLARE_OBJECT_NAME and then switch back to the original section
   afterwards.  */
#define SWITCH_TO_SECTION_FUNCTION 				\
void switch_to_section PARAMS ((enum in_section, tree));        \
void 								\
switch_to_section (section, decl) 				\
     enum in_section section; 					\
     tree decl; 						\
{ 								\
  switch (section) 						\
    { 								\
      case in_text: text_section (); break; 			\
      case in_data: data_section (); break; 			\
      case in_named: named_section (decl, NULL, 0); break; 	\
      case in_drectve: drectve_section (); break; 		\
      default: abort (); break; 				\
    } 								\
}

/* Don't allow flag_pic to propagate since gas may produce invalid code
   otherwise.  */

#undef  SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (flag_pic)								\
    {									\
      warning ("-f%s ignored for target (all code is position independent)",\
	       (flag_pic > 1) ? "PIC" : "pic");				\
      flag_pic = 0;							\
    }									\
} while (0)								\

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On i386 running Windows NT, modify the assembler name with a suffix 
   consisting of an atsign (@) followed by string of digits that represents
   the number of bytes of arguments passed to the function, if it has the 
   attribute STDCALL.

   In addition, we must mark dll symbols specially. Definitions of 
   dllexport'd objects install some info in the .drectve section.  
   References to dllimport'd objects are fetched indirectly via
   _imp__.  If both are declared, dllexport overrides.  This is also 
   needed to implement one-only vtables: they go into their own
   section and we need to set DECL_SECTION_NAME so we do that here.
   Note that we can be called twice on the same decl.  */

extern void i386_pe_encode_section_info PARAMS ((TREE));

#ifdef ENCODE_SECTION_INFO
#undef ENCODE_SECTION_INFO
#endif
#define ENCODE_SECTION_INFO(DECL) i386_pe_encode_section_info (DECL)

/* Utility used only in this file.  */
#define I386_PE_STRIP_ENCODING(SYM_NAME) \
  ((SYM_NAME) + ((SYM_NAME)[0] == '@' \
		  ? ((SYM_NAME)[3] == '*' ? 4 : 3) : 0) \
	      + ((SYM_NAME)[0] == '*' ? 1 : 0))

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  Discard
   trailing @[NUM] encoded by ENCODE_SECTION_INFO.  */
#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME)				\
do {									\
  const char *_p;							\
  const char *_name = I386_PE_STRIP_ENCODING (SYMBOL_NAME);		\
  for (_p = _name; *_p && *_p != '@'; ++_p)				\
    ;									\
  if (*_p == '@')							\
    {									\
      int _len = _p - _name;						\
      char *_new_name = (char *) alloca (_len + 1);			\
      strncpy (_new_name, _name, _len);					\
      _new_name[_len] = '\0';						\
      (VAR) = _new_name;						\
    }									\
  else									\
    (VAR) = _name;							\
} while (0)
      

/* Output a reference to a label.  */
#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)  		\
  fprintf (STREAM, "%s%s", USER_LABEL_PREFIX, 		\
	   I386_PE_STRIP_ENCODING (NAME))		\

/* Output a common block.  */
#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
do {							\
  if (i386_pe_dllexport_name_p (NAME))			\
    i386_pe_record_exported_symbol (NAME, 1);		\
  if (! i386_pe_dllimport_name_p (NAME))		\
    {							\
      fprintf ((STREAM), "\t.comm\t"); 			\
      assemble_name ((STREAM), (NAME));			\
      fprintf ((STREAM), ", %d\t%s %d\n",		\
	       (ROUNDED), ASM_COMMENT_START, (SIZE));	\
    }							\
} while (0)

/* Output the label for an initialized variable.  */
#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) 	\
do {							\
  if (i386_pe_dllexport_name_p (NAME))			\
    i386_pe_record_exported_symbol (NAME, 1);		\
  ASM_OUTPUT_LABEL ((STREAM), (NAME));			\
} while (0)


/* Emit code to check the stack when allocating more that 4000
   bytes in one go.  */

#define CHECK_STACK_LIMIT 4000

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387 and needs stack probes */
#undef TARGET_SUBTARGET_DEFAULT

#define TARGET_SUBTARGET_DEFAULT \
   (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_STACK_PROBE) 

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* Define this macro if in some cases global symbols from one translation
   unit may not be bound to undefined symbols in another translation unit
   without user intervention.  For instance, under Microsoft Windows
   symbols must be explicitly imported from shared libraries (DLLs).  */
#define MULTIPLE_SYMBOL_SPACES

extern void i386_pe_unique_section PARAMS ((TREE, int));
#define UNIQUE_SECTION(DECL,RELOC) i386_pe_unique_section (DECL, RELOC)

#define SUPPORTS_ONE_ONLY 1

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  i386_pe_asm_named_section

/* Select attributes for named sections.  */
#define TARGET_SECTION_TYPE_FLAGS  i386_pe_section_type_flags

/* Write the extra assembler code needed to declare a function
   properly.  If we are generating SDB debugging information, this
   will happen automatically, so we only need to handle other cases.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do									\
    {									\
      if (i386_pe_dllexport_name_p (NAME))				\
	i386_pe_record_exported_symbol (NAME, 0);			\
      if (write_symbols != SDB_DEBUG)					\
	i386_pe_declare_function_type (FILE, NAME, TREE_PUBLIC (DECL));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
    }									\
  while (0)

/* Add an external function to the list of functions to be declared at
   the end of the file.  */
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
  do									\
    {									\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	i386_pe_record_external_function (NAME);			\
    }									\
  while (0)

/* Declare the type properly for any external libcall.  */
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN) \
  i386_pe_declare_function_type (FILE, XSTR (FUN, 0), 1)

/* This says out to put a global symbol in the BSS section.  */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Output function declarations at the end of the file.  */
#undef ASM_FILE_END
#define ASM_FILE_END(FILE) \
  i386_pe_asm_file_end (FILE)

#undef ASM_COMMENT_START
#define ASM_COMMENT_START " #"

/* DWARF2 Unwinding doesn't work with exception handling yet.  To make it
   work, we need to build a libgcc_s.dll, and dcrt0.o should be changed to
   call __register_frame_info/__deregister_frame_info.  */
#define DWARF2_UNWIND_INFO 0

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

#undef PROFILE_HOOK
#define PROFILE_HOOK(LABEL)						\
  if (MAIN_NAME_P (DECL_NAME (current_function_decl)))			\
    {									\
      emit_call_insn (gen_rtx (CALL, VOIDmode,				\
	gen_rtx_MEM (FUNCTION_MODE,					\
		     gen_rtx_SYMBOL_REF (Pmode, "_monstartup")),	\
	const0_rtx));							\
    }

/* External function declarations.  */

extern void i386_pe_record_external_function PARAMS ((const char *));
extern void i386_pe_declare_function_type PARAMS ((FILE *, const char *, int));
extern void i386_pe_record_exported_symbol PARAMS ((const char *, int));
extern void i386_pe_asm_file_end PARAMS ((FILE *));
extern int i386_pe_dllexport_name_p PARAMS ((const char *));
extern int i386_pe_dllimport_name_p PARAMS ((const char *));

/* For Win32 ABI compatibility */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 128

/* Native complier aligns internal doubles in structures on dword boundaries.  */
#undef	BIGGEST_FIELD_ALIGNMENT
#define BIGGEST_FIELD_ALIGNMENT 64

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#undef PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS 1
#define GROUP_BITFIELDS_BY_ALIGN TYPE_NATIVE(rec)


/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set\t"
#endif

/* Override GCC's relative pathname lookup (ie., relocatability) unless
   otherwise told by other subtargets.  */
#ifndef WIN32_NO_ABSOLUTE_INST_DIRS
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX     "/usr/lib/"

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX     "/usr/lib/mingw/"

#ifndef CROSS_COMPILE
#undef LOCAL_INCLUDE_DIR
#undef TOOL_INCLUDE_DIR
#undef SYSTEM_INCLUDE_DIR
#undef STANDARD_INCLUDE_DIR
#define STANDARD_INCLUDE_DIR 0
#endif /* not CROSS_COMPILE */
#endif /* not WIN32_NO_ABSOLUTE_INST_DIRS */

#undef TREE

#ifndef BUFSIZ
# undef FILE
#endif
