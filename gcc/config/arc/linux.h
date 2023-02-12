/* Target macros for arc*-*-linux targets.

   Copyright (C) 2017-2023 Free Software Foundation, Inc.

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

/* Enable DWARF 2 exceptions.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      GNU_USER_TARGET_OS_CPP_BUILTINS ();	\
    }						\
  while (0)

#define GLIBC_DYNAMIC_LINKER					\
  "/lib/ld-linux-arc%{mbig-endian:eb}%{mcpu=arc700:700}.so.2"
#define UCLIBC_DYNAMIC_LINKER  "/lib/ld-uClibc.so.0"

/* Note that the default is to link against dynamic libraries, if they are
   available.  Override with -static.  */
#undef LINK_SPEC
#define LINK_SPEC "%{h*} \
  %{static:-Bstatic} \
  %{shared:-shared} \
  %{symbolic:-Bsymbolic} \
  %{!static: \
    %{rdynamic:-export-dynamic} \
    %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}} \
  -X \
  %{mbig-endian:-EB} %{EB} %{EL} \
  %{mcpu=nps400:-marclinux_nps; :-marclinux}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC							\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_STARTFILE_SPEC, ANDROID_STARTFILE_SPEC)

#undef ENDFILE_SPEC
#define ENDFILE_SPEC							\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_ENDFILE_SPEC, ANDROID_ENDFILE_SPEC)

#undef LIB_SPEC
#define LIB_SPEC  \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{profile:-lc_p}%{!profile:-lc}}"

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* No SDATA default for linux.  */
#undef TARGET_SDATA_DEFAULT
#define TARGET_SDATA_DEFAULT 0

/* We have medium calls.  */
#undef TARGET_MMEDIUM_CALLS_DEFAULT
#define TARGET_MMEDIUM_CALLS_DEFAULT 1

/* We do not have any MULTILIB_OPTIONS specified, so there are no
   MULTILIB_DEFAULTS.  */
#undef  MULTILIB_DEFAULTS

/* Linux toolchains use r25 as the thread pointer register.  */
#undef TARGET_ARC_TP_REGNO_DEFAULT
#define TARGET_ARC_TP_REGNO_DEFAULT 25

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{pthread:-D_REENTRANT} \
"

/* Indexed loads are default off.  */
#undef TARGET_INDEXED_LOADS_DEFAULT
#define TARGET_INDEXED_LOADS_DEFAULT 0

/* Pre/post modify with register displacement are default off.  */
#undef TARGET_AUTO_MODIFY_REG_DEFAULT
#define TARGET_AUTO_MODIFY_REG_DEFAULT 0

#if DEFAULT_LIBC == LIBC_GLIBC
/* Override linux.h LINK_EH_SPEC definition.
   Signalize that because we have fde-glibc, we don't need all C shared libs
   linked against -lgcc_s.  */
#undef LINK_EH_SPEC
#define LINK_EH_SPEC "--eh-frame-hdr "
#endif

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{pthread:-D_REENTRANT} \
"

/* Build attribute: procedure call standard.  */
#undef ATTRIBUTE_PCS
#define ATTRIBUTE_PCS 3

/* Clear the instruction cache from `beg' to `end'.  This makes an
   inline system call to SYS_cacheflush.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(beg, end)					\
{									\
  register unsigned long _beg __asm ("r0") = (unsigned long) (beg);	\
  register unsigned long _end __asm ("r1") = (unsigned long) (end);	\
  register unsigned long _xtr __asm ("r2") = 0;				\
  register unsigned long _scno __asm ("r8") = 244;			\
  __asm __volatile ("trap_s 0		; sys_cache_sync"		\
		    : "=r" (_beg)					\
		    : "0" (_beg), "r" (_end), "r" (_xtr), "r" (_scno));	\
}

/* Emit rtl for profiling.  Output assembler code to FILE
   to call "_mcount" for profiling a function entry.  */
#define PROFILE_HOOK(LABEL)					\
  {								\
   rtx fun, rt;							\
   rt = get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);	\
   fun = gen_rtx_SYMBOL_REF (Pmode, "_mcount");			\
   emit_library_call (fun, LCT_NORMAL, VOIDmode, rt, Pmode);	\
  }

/* Enter/Leave ops are default off for linux targets.  */
#undef TARGET_CODE_DENSITY_FRAME_DEFAULT
#define TARGET_CODE_DENSITY_FRAME_DEFAULT 0
