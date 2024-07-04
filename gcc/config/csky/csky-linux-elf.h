/* Declarations for C-SKY targets running Linux.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

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

/******************************************************************
 *		 Run-time Target Specification			  *
 ******************************************************************/

#undef STARTFILE_SPEC
#define STARTFILE_SPEC							      \
  "%{!shared: %{pie:Scrt1.o%s;:crt1.o%s}}				      \
  crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#undef CC1_SPEC
#define CC1_SPEC  \
  "%{EB:-EB}	  \
   %{EL:-EL}	  \
   %{profile:-p}  \
  "

#undef ASM_SPEC
#define ASM_SPEC		\
  "%{mbig-endian:-mbig-endian}	\
  %{EB:-EB}			\
  %{EL:-EL}			\
  %{fpic|fPIC:-pic}		\
  %{mcpu=*:-mcpu=%*}		\
  %{march=*:-march=%*}		\
  %{mhard-float:-mhard-float}	\
  %{mfloat-abi=softfp:-mhard-float} \
  %{mfloat-abi=hard:-mhard-float}   \
  %{melrw:-melrw}		\
  %{mno-elrw:-mno-elrw}		\
  %{mistack:-mistack}		\
  %{mno-istack:-mno-istack}	\
  %{mmp:-mmp}			\
  %{mcp:-mcp}			\
  %{mcache:-mcache}		\
  %{msecurity|mmac:-msecurity}	\
  %{mtrust:-mtrust}		\
  %{mdsp:-mdsp}			\
  %{medsp:-medsp}		\
  %{mvdsp:-mvdsp}		\
  "

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-cskyv2%{mfloat-abi=hard:-hf}%{mbig-endian:-be}.so.1"

#ifdef CSKY_ENABLE_MULTILIB
#undef SYSROOT_SUFFIX_SPEC
#define SYSROOT_SUFFIX_SPEC					\
  "%{mbig-endian:/big}"						\
  "%{mcpu=ck807*:/ck807}"					\
  "%{mcpu=ck860*:/ck860}"					\
  "%{mcpu=ck800*:/ck800}"					\
  "%{mfloat-abi=softfp:/soft-fp}"				\
  "%{mfloat-abi=hard:/hard-fp}"
#endif

#define LINUX_TARGET_LINK_SPEC	"%{h*} %{version:-v}		\
   %{b}								\
   %{static:-Bstatic}						\
   %{shared:-shared}						\
   %{symbolic:-Bsymbolic}					\
   %{!static:							\
     %{rdynamic:-export-dynamic}				\
     %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}	\
   -X								\
   %{mbig-endian:-EB} %{mlittle-endian:-EL}			\
   %{EB:-EB} %{EL:-EL}"


#undef	LINK_SPEC
#define LINK_SPEC LINUX_TARGET_LINK_SPEC


#undef	LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} -lc %{mccrt:-lcc-rt}"
/* FIXME add this to LIB_SPEC when need */
/*   %{!shared:%{profile:-lc_p}%{!profile:-lc}}" */

#define TARGET_OS_CPP_BUILTINS()	    \
  do					    \
    {					    \
      GNU_USER_TARGET_OS_CPP_BUILTINS ();   \
    }					    \
  while (0)

/* In crtstuff.c to control section in where code resides.
   We have to write it as asm code.  */
#ifdef __PIC__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)  \
  asm (SECTION_OP "\n"				    \
       "\tgrs\tr3, .Lgetpc_"#FUNC"\n\t"		    \
       ".Lgetpc_"#FUNC":\n\t"			    \
       "\tlrw\tr2,\t.Lgetpc_"#FUNC"@GOTPC\n\t"	    \
       "\taddu\tr3, r2\n\t"			    \
       "\tlrw\tr2, "#FUNC"@GOTOFF\n\t"		    \
       "\taddu\tr2, r3\n\t"			    \
       "\tjsr\tr2\n\t");			    \
  FORCE_CODE_SECTION_ALIGN			    \
  asm (TEXT_SECTION_ASM_OP);
#endif

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#undef FUNCTION_PROFILER
#define SAVE_LR	    \
  "push\tlr"
#define FUNCTION_PROFILER(file, labelno)		\
  fprintf (file, "\t%s\n\tjbsr\t_mcount\n", SAVE_LR);
#define NO_PROFILE_COUNTERS 1

/* Enable features only for Linux toolchains.  */
#define TARGET_CSKY_LINUX 1

/* Clear the instruction cache from `BEG' to `END'.  */
#define CLEAR_INSN_CACHE(BEG, END)			\
  cacheflush (BEG, END-BEG, 3)

/* For __clear_cache in libgcc2.c.  The declaration is copied from
   <sys/cachectl.h>.  */
#ifdef IN_LIBGCC2
extern int cacheflush (void *__addr, const int __nbytes, const int __op);
#endif

/* The SYNC operations are implemented as library functions, not
   INSN patterns.  As a result, the HAVE defines for the patterns are
   not defined.  We need to define them to generate the corresponding
   __GCC_HAVE_SYNC_COMPARE_AND_SWAP_* and __GCC_ATOMIC_*_LOCK_FREE
   defines.  */

#define HAVE_sync_compare_and_swapqi 1
#define HAVE_sync_compare_and_swaphi 1
#define HAVE_sync_compare_and_swapsi 1
