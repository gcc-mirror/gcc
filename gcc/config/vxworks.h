/* Common VxWorks target definitions for GNU compiler.
   Copyright (C) 1999-2018 Free Software Foundation, Inc.
   Contributed by Wind River Systems.
   Rewritten by CodeSourcery, LLC.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Assert that we are targeting VxWorks.  */
#undef TARGET_VXWORKS
#define TARGET_VXWORKS 1

/* If TARGET_VXWORKS7 is undefined, then we're not targeting it.  */
#ifndef TARGET_VXWORKS7
#define TARGET_VXWORKS7 0
#endif

/* In kernel mode, VxWorks provides all the libraries itself, as well as
   the functionality of startup files, etc.  In RTP mode, it behaves more
   like a traditional Unix, with more external files.  Most of our specs
   must be aware of the difference.  */

/* We look for the VxWorks header files using the environment
   variables that are set in VxWorks to indicate the location of the
   system header files.  We use -idirafter so that the GCC's own
   header-file directories (containing <stddef.h>, etc.) come before
   the VxWorks system header directories.  */

/* Since we provide a default -isystem, expand -isystem on the command
   line early.  */
#if TARGET_VXWORKS7

#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC                     \
 "%{!nostdinc:                                          \
    %{isystem*}                                         \
    %{mrtp: -idirafter %:getenv(VSB_DIR /h)             \
            -idirafter %:getenv(VSB_DIR /share/h)       \
            -idirafter %:getenv(VSB_DIR /usr/h/public)  \
            -idirafter %:getenv(VSB_DIR /usr/h)         \
      ;:    -idirafter %:getenv(VSB_DIR /h)             \
            -idirafter %:getenv(VSB_DIR /share/h)       \
            -idirafter %:getenv(VSB_DIR /krnl/h/system) \
            -idirafter %:getenv(VSB_DIR /krnl/h/public)}}"

#else /* TARGET_VXWORKS7 */

#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC		\
 "%{!nostdinc:					\
    %{isystem*}					\
    %{mrtp: -idirafter %:getenv(WIND_USR /h)	\
	    -idirafter %:getenv(WIND_USR /h/wrn/coreip) \
      ;:    -idirafter %:getenv(WIND_BASE /target/h) \
	    -idirafter %:getenv(WIND_BASE /target/h/wrn/coreip) \
}}"

#endif

/* The references to __init and __fini will be satisfied by
   libc_internal.a, and some versions of VxWorks rely on explicit
   extra libraries for system calls.  */

#define VXWORKS_SYSCALL_LIBS_RTP

#define VXWORKS_LIBS_RTP \
  VXWORKS_SYSCALL_LIBS_RTP " -lc -lgcc -lc_internal -lnet -ldsi"

/* On Vx6 and previous, the libraries to pick up depends on the architecture,
   so cannot be defined for all archs at once.  On Vx7, a VSB is always needed
   and its structure is fixed and does not depend on the arch.  We can thus
   tell gcc where to look for when linking with RTP libraries.  */

/* On Vx7 RTP, we need to drag the __tls__ symbol to trigger initialization of
   tlsLib, responsible for TLS support by the OS.  */

#if TARGET_VXWORKS7
#define VXWORKS_LIBS_DIR_RTP "-L%:getenv(VSB_DIR /usr/lib/common)"
#define TLS_SYM "-u __tls__"
#else
#define VXWORKS_LIBS_DIR_RTP ""
#define TLS_SYM ""
#endif

#undef VXWORKS_LIB_SPEC
#define	VXWORKS_LIB_SPEC						\
"%{mrtp:%{shared:-u " USER_LABEL_PREFIX "__init -u " USER_LABEL_PREFIX "__fini} \
	%{!shared:%{non-static:-u " USER_LABEL_PREFIX "_STI__6__rtld -ldl} \
		  " TLS_SYM " \
		  --start-group " VXWORKS_LIBS_RTP " --end-group} \
        " VXWORKS_LIBS_DIR_RTP "}"

/* The no-op spec for "-shared" below is present because otherwise GCC
   will treat it as an unrecognized option.  */
#undef VXWORKS_LINK_SPEC
#define VXWORKS_LINK_SPEC				\
"%{!mrtp:-r}						\
 %{!shared:						\
   %{mrtp:-q %{h*}					\
          %{R*} %{!T*: %(link_start) }			\
          %(link_target) %(link_os)}}			\
 %{v:-v}						\
 %{shared:-shared}					\
 %{Bstatic:-Bstatic}					\
 %{Bdynamic:-Bdynamic}					\
 %{!Xbind-lazy:-z now}					\
 %{Xbind-now:%{Xbind-lazy:				\
   %e-Xbind-now and -Xbind-lazy are incompatible}}	\
 %{mrtp:%{!shared:%{!non-static:-static}		\
 		  %{non-static:--force-dynamic --export-dynamic}}}"

/* For VxWorks static rtps, the system provides libc_internal.a, a superset
   of libgcc.a that we want to use.  Make sure not to dynamically export any
   of its symbols, though, and always look for libgcc.a first so that we get
   the latest versions of the GNU intrinsics during our builds.  */
#undef VXWORKS_LIBGCC_SPEC
#define VXWORKS_LIBGCC_SPEC \
  "-lgcc %{mrtp:%{!shared:--exclude-libs=libc_internal,libgcc -lc_internal}}"

#undef VXWORKS_STARTFILE_SPEC
#define	VXWORKS_STARTFILE_SPEC "%{mrtp:%{!shared:-l:crt0.o}}"
#define VXWORKS_ENDFILE_SPEC ""

/* Do VxWorks-specific parts of TARGET_OPTION_OVERRIDE.  */

#define VXWORKS_HAVE_TLS (TARGET_VXWORKS7 && TARGET_VXWORKS_RTP)

#undef VXWORKS_OVERRIDE_OPTIONS
#define VXWORKS_OVERRIDE_OPTIONS vxworks_override_options ()
extern void vxworks_override_options (void);

/* RTPs support prioritized constructors and destructors: the
   implementation relies on numbered .ctors* sections. If the compiler
   was built with --enable-initfini-array, we assume the user uses a
   linker script that sorts and merges the .init_array.* sections
   appropriately.  */
#define SUPPORTS_INIT_PRIORITY \
  (TARGET_VXWORKS_RTP || HAVE_INITFINI_ARRAY_SUPPORT)

#if !HAVE_INITFINI_ARRAY_SUPPORT
/* VxWorks requires special handling of constructors and destructors.
   All VxWorks configurations must use these functions.  */
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR vxworks_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR vxworks_asm_out_destructor
extern void vxworks_asm_out_constructor (rtx symbol, int priority);
extern void vxworks_asm_out_destructor (rtx symbol, int priority);
#endif

/* Override the vxworks-dummy.h definitions.  TARGET_VXWORKS_RTP
   is defined by vxworks.opt.  */
#undef VXWORKS_GOTT_BASE
#define VXWORKS_GOTT_BASE "__GOTT_BASE__"
#undef VXWORKS_GOTT_INDEX
#define VXWORKS_GOTT_INDEX "__GOTT_INDEX__"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

/* Both kernels and RTPs have the facilities required by this macro.  */
#define TARGET_POSIX_IO

/* A VxWorks implementation of TARGET_OS_CPP_BUILTINS.  */
#define VXWORKS_OS_CPP_BUILTINS()					\
  do									\
    {									\
      builtin_define ("__vxworks");					\
      builtin_define ("__VXWORKS__");					\
      builtin_assert ("system=unix");					\
      if (TARGET_VXWORKS_RTP)						\
	builtin_define ("__RTP__");					\
      else								\
	builtin_define ("_WRS_KERNEL");					\
      builtin_define ("_VX_TOOL_FAMILY=gnu");				\
      builtin_define ("_VX_TOOL=gnu");					\
      if (TARGET_VXWORKS7)						\
        {								\
           builtin_define ("_VSB_CONFIG_FILE=<config/vsbConfig.h>");	\
           								\
	   /* _ALLOW_KEYWORD_MACROS is needed on VxWorks 7 to		\
	      prevent compilation failures triggered by our		\
	      definition of "inline" in ansidecl when "inline"		\
	      is not a keyword.  */					\
	   if (!flag_isoc99 && !c_dialect_cxx())			\
             builtin_define ("_ALLOW_KEYWORD_MACROS");			\
        }								\
    }									\
  while (0)

#define VXWORKS_KIND VXWORKS_KIND_NORMAL

/* The diab linker does not handle .gnu_attribute sections.  */
#undef HAVE_AS_GNU_ATTRIBUTE

/* Default dwarf control values, for non-gdb debuggers that come with
   VxWorks.  */

#undef VXWORKS_DWARF_VERSION_DEFAULT
#define VXWORKS_DWARF_VERSION_DEFAULT (TARGET_VXWORKS7 ? 4 : 2)

#undef DWARF_GNAT_ENCODINGS_DEFAULT
#define DWARF_GNAT_ENCODINGS_DEFAULT \
  (TARGET_VXWORKS7 ? DWARF_GNAT_ENCODINGS_MINIMAL : DWARF_GNAT_ENCODINGS_ALL)
