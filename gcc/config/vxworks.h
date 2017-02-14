/* Common VxWorks target definitions for GNU compiler.
   Copyright (C) 1999-2017 Free Software Foundation, Inc.
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
#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC		\
 "%{!nostdinc:					\
    %{isystem*} -idirafter			\
    %{mrtp: %:getenv(WIND_USR /h)		\
      ;:    %:getenv(WIND_BASE /target/h)}}"

/* The references to __init and __fini will be satisfied by
   libc_internal.a.  */
#undef VXWORKS_LIB_SPEC
#define	VXWORKS_LIB_SPEC						\
"%{mrtp:%{shared:-u " USER_LABEL_PREFIX "__init -u " USER_LABEL_PREFIX "__fini} \
	%{!shared:%{non-static:-u " USER_LABEL_PREFIX "_STI__6__rtld -ldl} \
		  --start-group -lc -lgcc -lc_internal -lnet -ldsi	\
		  --end-group}}"

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
#undef VXWORKS_OVERRIDE_OPTIONS
#define VXWORKS_OVERRIDE_OPTIONS vxworks_override_options ()
extern void vxworks_override_options (void);

/* Only RTPs support prioritized constructors and destructors:
   the implementation relies on numbered .ctors* sections.  */
#define SUPPORTS_INIT_PRIORITY TARGET_VXWORKS_RTP

/* VxWorks requires special handling of constructors and destructors.
   All VxWorks configurations must use these functions.  */
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR vxworks_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR vxworks_asm_out_destructor
extern void vxworks_asm_out_constructor (rtx symbol, int priority);
extern void vxworks_asm_out_destructor (rtx symbol, int priority);

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
    }									\
  while (0)

#define VXWORKS_KIND VXWORKS_KIND_NORMAL

/* The diab linker does not handle .gnu_attribute sections.  */
#undef HAVE_AS_GNU_ATTRIBUTE
