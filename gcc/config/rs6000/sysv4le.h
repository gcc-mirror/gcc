/* Target definitions for GNU compiler for a little endian PowerPC
   running System V.4
   Copyright (C) 1995, 2000 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_LITTLE_ENDIAN)

#undef	CPP_ENDIAN_DEFAULT_SPEC
#define	CPP_ENDIAN_DEFAULT_SPEC "%(cpp_endian_little)"

#undef	CC1_ENDIAN_DEFAULT_SPEC
#define	CC1_ENDIAN_DEFAULT_SPEC "%(cc1_endian_little)"

#undef	LINK_TARGET_SPEC
#define	LINK_TARGET_SPEC "\
%{mbig: --oformat elf32-powerpc } %{mbig-endian: --oformat elf32-powerpc } \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-linux: --oformat elf32-powerpc} \
  }}}}"

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mlittle", "mcall-sysv" }
