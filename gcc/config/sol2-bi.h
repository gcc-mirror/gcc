/* Definitions of target machine for GCC, for bi-arch Solaris 2.
   Copyright (C) 2011 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* wchar_t is called differently in <wchar.h> for 32 and 64-bit
   compilations.  This is called for by SCD 2.4.1, p. 6-83, Figure 6-65
   (32-bit) and p. 6P-10, Figure 6.38 (64-bit).  */

#undef WCHAR_TYPE
#define WCHAR_TYPE (TARGET_64BIT ? "int" : "long int")

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Same for wint_t.  See SCD 2.4.1, p. 6-83, Figure 6-66 (32-bit).  There's
   no corresponding 64-bit definition, but this is what Solaris 8
   <iso/wchar_iso.h> uses.  */

#undef WINT_TYPE
#define WINT_TYPE (TARGET_64BIT ? "int" : "long int")

#undef WINT_TYPE_SIZE
#define WINT_TYPE_SIZE 32

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

#if DEFAULT_ARCH32_P
#define DEF_ARCH32_SPEC(__str) "%{!m64:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{m64:" __str "}"
#else
#define DEF_ARCH32_SPEC(__str) "%{m32:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{!m32:" __str "}"
#endif

#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" ASM_CPU64_DEFAULT_SPEC "} \
%{!m64:" ASM_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" ASM_CPU32_DEFAULT_SPEC "} \
%{!m32:" ASM_CPU64_DEFAULT_SPEC "} \
")

/* This should be the same as LINK_ARCH32_SPEC_BASE, except with
   ARCH64_SUBDIR appended to the paths and /usr/ccs/lib is no longer
   necessary.  */
#undef LINK_ARCH64_SPEC_BASE
#define LINK_ARCH64_SPEC_BASE \
  "%{G:-G} \
   %{YP,*} \
   %{R*} \
   %{!YP,*:%{p|pg:-Y P,%R/usr/lib/libp/" ARCH64_SUBDIR ":%R/lib/" ARCH64_SUBDIR ":%R/usr/lib/" ARCH64_SUBDIR "}	\
	   %{!p:%{!pg:-Y P,%R/lib/" ARCH64_SUBDIR ":%R/usr/lib/" ARCH64_SUBDIR "}}}"

#undef LINK_ARCH64_SPEC
#ifndef USE_GLD
/* FIXME: Used to be SPARC-only.  Not SPARC-specfic but for the model name!  */
#define LINK_ARCH64_SPEC \
  "%{mcmodel=medlow:-M /usr/lib/ld/" ARCH64_SUBDIR "/map.below4G} " \
  LINK_ARCH64_SPEC_BASE
#else
#define LINK_ARCH64_SPEC LINK_ARCH64_SPEC_BASE
#endif

#ifdef USE_GLD
#if DEFAULT_ARCH32_P
#define ARCH_DEFAULT_EMULATION ARCH32_EMULATION
#else
#define ARCH_DEFAULT_EMULATION ARCH64_EMULATION
#endif
#define TARGET_LD_EMULATION "%{m32:-m " ARCH32_EMULATION "}" \
			    "%{m64:-m " ARCH64_EMULATION "}" \
			    "%{!m32:%{!m64:-m " ARCH_DEFAULT_EMULATION "}} "
#else
#define TARGET_LD_EMULATION ""
#endif

#undef LINK_ARCH_SPEC
#if DISABLE_MULTILIB
#if DEFAULT_ARCH32_P
#define LINK_ARCH_SPEC TARGET_LD_EMULATION " \
%{m32:%(link_arch32)} \
%{m64:%edoes not support multilib} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#else
#define LINK_ARCH_SPEC TARGET_LD_EMULATION " \
%{m32:%edoes not support multilib} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#endif
#else
#define LINK_ARCH_SPEC TARGET_LD_EMULATION " \
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}}"
#endif

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "startfile_arch",	 STARTFILE_ARCH_SPEC },		\
  { "link_arch32",       LINK_ARCH32_SPEC },            \
  { "link_arch64",       LINK_ARCH64_SPEC },            \
  { "link_arch_default", LINK_ARCH_DEFAULT_SPEC },	\
  { "link_arch",	 LINK_ARCH_SPEC },		\
  SUBTARGET_CPU_EXTRA_SPECS
