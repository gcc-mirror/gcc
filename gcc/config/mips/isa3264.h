/* Definitions of target machine for GNU compiler.
   MIPS 32 and MIPS 64 version.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

#ifndef MIPS_ISA_DEFAULT
#define MIPS_ISA_DEFAULT 32
#endif

#ifndef MIPS_ABI_DEFAULT
#define MIPS_ABI_DEFAULT ABI_MEABI
#endif

#ifndef MIPS_ENABLE_EMBEDDED_O32
#define MIPS_ENABLE_EMBEDDED_O32 1
#endif

#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

#include "mips/elf.h"

/* This must be done after including mips.h so that the
   ABI_{EABI,O64,O32,...} are #defined.  */

#if MIPS_ABI_DEFAULT == ABI_EABI
#undef  SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=eabi|!mabi=*:\
  %{mips1|mips2|mips32|mlong32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
  %{!mips1:%{!mips2:%{!mips32:%{!mlong32:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int}}}}} \
%{mabi=o64:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}} \
%{mabi=32:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
"
#endif

#if MIPS_ABI_DEFAULT == ABI_O64
#undef  SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=eabi:\
  %{mips1|mips2|mips32|mlong32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
  %{!mips1:%{!mips2:%{!mips32:%{!mlong32:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int}}}}} \
%{mabi=o64|!mabi=*:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}} \
%{mabi=32:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
"
#endif

#if MIPS_ABI_DEFAULT == ABI_32
#if MIPS_ISA_DEFAULT == 3 || MIPS_ISA_DEFAULT == 4 || MIPS_ISA_DEFAULT == 5 || MIPS_ISA_DEFAULT == 64
#undef  SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=eabi:\
  %{mips1|mips2|mips32|mlong32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
  %{!mips1:%{!mips2:%{!mips32:%{!mlong32:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int}}}}} \
%{mabi=o64|!!mabi=*:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}} \
%{mabi=32:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
"
#else /* not a 64bit default ISA */
#undef  SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=eabi:\
  %{mips3|mips4|mips5|mips64|mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
  %{!mips3:%{!mips4:%{!mips5:%{!mips64:%{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}}}}}} \
%{mabi=o64:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}} \
%{mabi=32:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{!mabi=*:\
 %{mips3|mips4|mips5|mips64:\
  %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
  %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}} \
 %{!mips3:%{!mips4:%{!mips5:%{!mips64:\
  -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}}}}} \
"
#endif /* ISA */
#endif

#if MIPS_ABI_DEFAULT == ABI_MEABI
/* For MEABI, don't link with crt0 files, let the linker start files specify
   the appropriate crt0 file.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crti%O%s crtbegin%O%s %{!mno-crt0: }"

/*
   The MIPS_ISA_DEFAULT test is for EABI, in which the size of longs depends on
   the ISA.

   For MEABI the size of longs is always 32bits.  If long64 is specified then
   we honor that.  The errors for long64 & long32 is because while CC1 can
   handle overriding mlong32 with mlong64 and vise-versa, the specs cannot.  */

#if MIPS_ISA_DEFAULT == 3 || MIPS_ISA_DEFAULT == 4 || MIPS_ISA_DEFAULT == 5 || MIPS_ISA_DEFAULT == 64
#undef  SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=meabi:\
  %{mips1|mips2|mips32|mlong32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
  %{!mips1:%{!mips2:%{!mips32:%{!mlong32:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int}}}}} \
%{mabi=o64:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int \
  %{mlong32:%e-mlong32 and -mlong64 can not both be specified}} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int }} \
%{mabi=32:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=meabi|!mabi=*:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int \
  %{mlong32:%e-mlong32 and -mlong64 can not both be specified}} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int }} \
"

#else /* not a 64bit default ISA */
#undef  SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=meabi:\
  %{mips3|mips4|mips5|mips64|mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
  %{!mips3:%{!mips4:%{!mips5:%{!mips64:%{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}}}}}} \
%{mabi=o64:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int \
  %{mlong32:%e-mlong32 and -mlong64 can not both be specified}} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int }} \
%{mabi=32:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=meabi|!mabi=*:\
 %{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int \
  %{mlong32:%e-mlong32 and -mlong64 can not both be specified}} \
 %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int }} \
"
#endif /* ISA */
#endif

