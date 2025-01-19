/* Definitions of target machine of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2025 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* ------------------------------------------------------------------------ */

#define TARGET_LINUX_ABI 0

/* In the configure stage we may use options --enable-default-relax,
   --enable-Os-default-ifc and --enable-Os-default-ex9.  They effect
   the default spec of passing --relax, --mifc, and --mex9 to linker.
   We use NDS32_RELAX_SPEC, NDS32_IFC_SPEC, and NDS32_EX9_SPEC
   so that we can customize them conveniently.  */
#define LINK_SPEC \
  " %{G*}" \
  " %{mbig-endian:-EB} %{mlittle-endian:-EL}" \
  " %{shared:-shared}" \
  NDS32_RELAX_SPEC

#define LIB_SPEC \
  " -lc -lgloss"

#define LIBGCC_SPEC \
  " -lgcc"

/* The option -mno-ctor-dtor can disable constructor/destructor feature
   by applying different crt stuff.  In the convention, crt0.o is the
   startup file without constructor/destructor;
   crt1.o, crti.o, crtbegin.o, crtend.o, and crtn.o are the
   startup files with constructor/destructor.
   Note that crt0.o, crt1.o, crti.o, and crtn.o are provided
   by newlib/mculib/glibc/ublic, while crtbegin.o and crtend.o are
   currently provided by GCC for nds32 target.

   For nds32 target so far:
   If -mno-ctor-dtor, we are going to link
   "crt0.o [user objects]".
   If -mctor-dtor, we are going to link
   "crt1.o crtbegin1.o [user objects] crtend1.o".

   Note that the TARGET_DEFAULT_CTOR_DTOR would effect the
   default behavior.  Check gcc/config.gcc for more information.  */
#ifdef TARGET_DEFAULT_CTOR_DTOR
  #define STARTFILE_SPEC \
    " %{!mno-ctor-dtor:crt1.o%s;:crt0.o%s}" \
    " %{!mno-ctor-dtor:crtbegin1.o%s}" \
    " %{mcrt-arg:crtarg.o%s}"
  #define ENDFILE_SPEC \
    " %{!mno-ctor-dtor:crtend1.o%s}"
#else
  #define STARTFILE_SPEC \
    " %{mctor-dtor|coverage:crt1.o%s;:crt0.o%s}" \
    " %{mctor-dtor|coverage:crtbegin1.o%s}" \
    " %{mcrt-arg:crtarg.o%s}"
  #define ENDFILE_SPEC \
    " %{mctor-dtor|coverage:crtend1.o%s}"
#endif

#define STARTFILE_CXX_SPEC \
  " %{!mno-ctor-dtor:crt1.o%s;:crt0.o%s}" \
  " %{!mno-ctor-dtor:crtbegin1.o%s}" \
  " %{mcrt-arg:crtarg.o%s}"
#define ENDFILE_CXX_SPEC \
  " %{!mno-ctor-dtor:crtend1.o%s}"
