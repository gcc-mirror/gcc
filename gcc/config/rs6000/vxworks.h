/* Definitions of target machine for GNU compiler.  Vxworks PowerPC version.
   Copyright (C) 1996, 2000, 2002, 2003 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the
   Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__vxworks");		\
      builtin_define ("__vxworks__");		\
    }						\
  while (0)

/* We have to kill off the entire specs set created by rs6000/sysv4.h
   and substitute our own set.  The top level vxworks.h has done some
   of this for us.  */

#undef SUBTARGET_EXTRA_SPECS
#undef CPP_SPEC
#undef CC1_SPEC
#undef ASM_SPEC

#define SUBTARGET_EXTRA_SPECS /* none needed */

#define CPP_SPEC \
"-DCPU_FAMILY=PPC -D__ppc -D__EABI__  \
 %{t403: -DCPU=PPC403 -D_SOFT_FLOAT ; \
   t405: -DCPU=PPC405 -D_SOFT_FLOAT ; \
   t440: -DCPU=PPC440 -D_SOFT_FLOAT ; \
   t603: -DCPU=PPC603               ; \
   t604: -DCPU=PPC604               ; \
   t860: -DCPU=PPC860 -D_SOFT_FLOAT ; \
       : -DCPU=PPC604}  \
 %{!msoft-float:-D__hardfp}	   \
 %{fpic|fpie: -D__PIC__=1 -D__pic__=1 ; \
   fPIC|fPIE: -D__PIC__=2 -D__pic__=2 } \
 %(cpp_cpu)"

#define CC1_SPEC \
"%{t403: -mcpu=403 -mstrict-align ;				\
   t405: -mcpu=405 -mstrict-align ;				\
   t440: -mcpu=440 -mstrict-align ;				\
   t603: -mcpu=603 -mstrict-align ;				\
   t604: -mcpu=604 -mstrict-align ;				\
   t860: -mcpu=860                ;                             \
       : -mcpu=604 -mstrict-align }				\
 %{G*} %{mno-sdata:-msdata=none} %{msdata:-msdata=default}	\
 %{mlittle|mlittle-endian:-mstrict-align}			\
 %{profile: -p}							\
 %{fvec:-maltivec} %{fvec-eabi:-maltivec -mabi=altivec}"
   
#define ASM_SPEC "%(asm_cpu) \
%{.s: %{mregnames} %{mno-regnames}} %{.S: %{mregnames} %{mno-regnames}} \
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} \
%{mrelocatable} %{mrelocatable-lib} %{fpic:-K PIC} %{fPIC:-K PIC} -mbig"

#undef  MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "t604" }

/* We can't use .ctors/.dtors sections.  */
#undef TARGET_ASM_OUTPUT_CONSTRUCTOR
#undef TARGET_ASM_OUTPUT_DESTRUCTOR

/* Nor sdata.  */
#undef  SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE 0
