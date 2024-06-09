/* Common VxWorks AE target definitions for GNU compiler.
   Copyright (C) 2004-2024 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

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

/* This header should be included after including vx-common.h.  */

/* Most of the definitions below this point are versions of the
   vxworks.h definitions, without the -mrtp bits.  */

/* Resolve subdirectory of VxWorks AE target headers.  */
#define VXWORKSAE_TARGET_DIR(SUBDIR) "%:getenv(WIND_BASE /target" SUBDIR ")"

/* Include target/vThreads/h or target/h (depending on the compilation
   mode), and then target/val/h (in either mode).  The macros defined
   are in the user's namespace, but the VxWorks headers require
   them.  */
#undef VXWORKS_ADDITIONAL_CPP_SPEC
#define VXWORKS_ADDITIONAL_CPP_SPEC "					\
 %{!nostdinc:%{isystem*}}						\
 %{mvthreads:-DVTHREADS=1						\
	 %{!nostdinc:-isystem " VXWORKSAE_TARGET_DIR("/vThreads/h") "}}	\
 %{!mvthreads:-DAE653_BUILD=1						\
	 %{!nostdinc:-isystem " VXWORKSAE_TARGET_DIR("/h") "}}		\
 %{!nostdinc:-isystem " VXWORKSAE_TARGET_DIR("/val/h") "}"

#undef VXWORKS_LIB_SPEC
#define VXWORKS_LIB_SPEC ""

#undef VXWORKS_LINK_SPEC
#define VXWORKS_LINK_SPEC	\
  "-r %{v:-V}"
 
#undef VXWORKS_LIBGCC_SPEC
#define VXWORKS_LIBGCC_SPEC	\
  "-lgcc"

/* The VxWorks AE ports features are restricted on purpose.  No RTPs,
   for example.  */

#undef TARGET_VXWORKS_HAVE_CTORS_DTORS
#define TARGET_VXWORKS_HAVE_CTORS_DTORS 0

#undef VXWORKS_STARTFILE_SPEC
#define VXWORKS_STARTFILE_SPEC ""

#define VXWORKS_KIND VXWORKS_KIND_AE

/* Both kernels and RTPs have the facilities required by this macro.  */
#define TARGET_POSIX_IO

/* The AE/653 system headers all predate the introduction of _VX_ prefixes
   ahead of CPU families of macros.  */
#define VX_CPU_PREFIX ""

/* A VxWorks 653 implementation of TARGET_OS_CPP_BUILTINS.  */
#define VXWORKS_OS_CPP_BUILTINS()                                       \
  do                                                                    \
    {                                                                   \
      builtin_define ("__vxworks");                                     \
      builtin_define ("__VXWORKS__");                                   \
    }                                                                   \
  while (0)

/* Do VxWorks-specific parts of TARGET_OPTION_OVERRIDE.  */

/* None of the VxWorks AE/653/MILS ports to date has native TLS support.  */
#define VXWORKS_HAVE_TLS 0

#undef VXWORKS_OVERRIDE_OPTIONS
#define VXWORKS_OVERRIDE_OPTIONS vxworks_override_options ()
extern void vxworks_override_options (void);

/* Default dwarf control values, for non-gdb debuggers that come with
   VxWorks.  */

#undef VXWORKS_DWARF_VERSION_DEFAULT
#define VXWORKS_DWARF_VERSION_DEFAULT 2

#undef DWARF_GNAT_ENCODINGS_DEFAULT
#define DWARF_GNAT_ENCODINGS_DEFAULT DWARF_GNAT_ENCODINGS_ALL
