/* Definitions of target machine for GNU compiler.  IRIX version 6.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007
   Free Software Foundation, Inc.

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

/* Allow some special handling for IRIX 6.  */
#undef TARGET_IRIX6
#define TARGET_IRIX6 1

/* Default to -mabi=n32 and -mips3.  */
#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mabi=n32" }

/* Force the default ABI onto the command line in order to make the specs
   easier to write.  Default to the mips2 ISA for the O32 ABI.  */
#define DRIVER_SELF_SPECS \
  "%{!mabi=*: -mabi=n32}", \
  "%{mabi=32: %{!mips*: %{!march*: -mips2}}}"

/* Force the generation of dwarf .debug_frame sections even if not
   compiling -g.  This guarantees that we can unwind the stack.  */
#define DWARF2_FRAME_INFO 1

/* The system unwinder in libexc requires a specific dwarf return address
   column to work.  */
#undef  DWARF_FRAME_RETURN_COLUMN
#define DWARF_FRAME_RETURN_COLUMN (FP_REG_LAST + 1)

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 6.x"

#ifdef IRIX_USING_GNU_LD
#define IRIX_SUBTARGET_LINK_SPEC \
  "%{mabi=32: -melf32bsmip}%{mabi=n32: -melf32bmipn32}%{mabi=64: -melf64bmip}"
#else
  /* Explicitly hide crt symbols that would normally be marked with
     a "hidden" visibility attribute.
     
     We have traditionally disabled this attribute when using the
     native linker because the native linker's visibility support is
     not fully-compatible with the GNU linker's.  In particular, the
     native linker does not pull in archive objects purely to resolve
     references to the object's hidden symbols, whereas the GNU
     linker does.
     
     The gcc build system currently hides symbols in some static
     libraries (typically libgcov.a or libgcc.a) whenever visibility
     attributes are supported.  On targets with GNU semantics, this
     makes sure that uses of libx.so symbols in one dynamic object are
     not resolved to libx.a symbols in another dynamic object.  But
     on targets with IRIX semantics, hiding the symbols prevents the
     static archive from working at all.
     
     It would probably be better to enable visiblity attributes for
     IRIX ld and disable the static archives versioning.  It shouldn't
     make anything worse, since libx.a symbols are global by default
     anyway.  However, no-one has volunteered to do this yet.  */

#define IRIX_SUBTARGET_LINK_SPEC \
  "%{w} -_SYSTYPE_SVR4 -woff 131 \
   %{shared:-hidden_symbol __dso_handle} \
   %{mabi=32: -32}%{mabi=n32: -n32}%{mabi=64: -64}%{!mabi*: -n32}"
#endif

/* Profiling is supported via libprof1.a not -lc_p as in IRIX 3.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{mabi=32:%{pg:gcrt1.o%s} \
       %{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}} \
     %{mabi=n32: \
       %{mips4:%{pg:/usr/lib32/mips4/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/mcrt1.o%s /usr/lib32/mips4/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/mcrt1.o%s /usr/lib32/mips3/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/crt1.o%s}}}} \
     %{mabi=64: \
       %{mips4:%{pg:/usr/lib64/mips4/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips4/mcrt1.o /usr/lib64/mips4/libprof1.a} \
           %{!p:/usr/lib64/mips4/crt1.o}}} \
       %{!mips4:%{pg:/usr/lib64/mips3/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips3/mcrt1.o /usr/lib64/mips3/libprof1.a} \
           %{!p:/usr/lib64/mips3/crt1.o}}}}} \
  irix-crti.o%s crtbegin.o%s"

#ifdef IRIX_USING_GNU_LD
#define SUBTARGET_DONT_WARN_UNUSED_SPEC ""
#define SUBTARGET_WARN_UNUSED_SPEC ""
#else
#define SUBTARGET_DONT_WARN_UNUSED_SPEC "-dont_warn_unused"
#define SUBTARGET_WARN_UNUSED_SPEC "-warn_unused"
#endif

#undef LIB_SPEC
#define LIB_SPEC \
  "%{mabi=n32: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{mabi=64: %{mips4:-L/usr/lib64/mips4} %{!mips4:-L/usr/lib64/mips3} \
     -L/usr/lib64} \
   %{!shared:" \
     SUBTARGET_DONT_WARN_UNUSED_SPEC \
     " %{pthread:-lpthread} %{p:libprof1.a%s}%{pg:libprof1.a%s} -lc " \
     SUBTARGET_WARN_UNUSED_SPEC "}"

/* Avoid getting two warnings for libgcc.a everytime we link.  libgcc.a
   contains references to copysignl, so link with libm to resolve them.  */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC \
  SUBTARGET_DONT_WARN_UNUSED_SPEC " -lgcc -lm " SUBTARGET_WARN_UNUSED_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend.o%s irix-crtn.o%s \
   %{!shared: \
     %{mabi=32:crtn.o%s}\
     %{mabi=n32:%{mips4:/usr/lib32/mips4/crtn.o%s}\
       %{!mips4:/usr/lib32/mips3/crtn.o%s}}\
     %{mabi=64:%{mips4:/usr/lib64/mips4/crtn.o%s}\
       %{!mips4:/usr/lib64/mips3/crtn.o%s}}}"

#define MIPS_TFMODE_FORMAT mips_extended_format

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "%{pthread:-D_REENTRANT}"

