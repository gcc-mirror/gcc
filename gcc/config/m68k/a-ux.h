/* Definitions for Motorola 680x0 running A/UX
   Copyright (C) 1996, 1998, 1999 Free Software Foundation, Inc.

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

/* This file was renamed from aux.h because of MSDOS: aux.anything
   isn't usable.  Sigh.  */

/* Execution environment */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68881|MASK_68020)	/* 68020, 68881 */

#define CPP_PREDEFINES "-Dunix -Dm68k -DAUX -DmacII \
-Asystem=unix -Asystem=AUX -Acpu=m68k -Amachine=m68k -Amachine=macII"

#define CPP_SPEC \
"%{!msoft-float:%{!ansi:-Dmc68881 }-D__HAVE_68881__ }\
-Acpu=mc68000 -D__mc68000__ %{!ansi:-Dmc68000 }\
%{!mc68000:%{!m68000:-Acpu=mc68020 -D__mc68020__ %{!ansi:-Dmc68020 }}}\
%{m68030:-Acpu=mc68030 -D__mc68030__ %{!ansi:-Dmc68030 }}\
%{m68040:-Acpu=mc68040 -D__mc68040__ %{!ansi:-Dmc68040 }}\
%{!ansi:%{!traditional:-D__STDC__=2 }}\
%{sbsd:-D_BSD_SOURCE -DBSD }%{ZB:-D_BSD_SOURCE -DBSD }\
%{ssysv:-D_SYSV_SOURCE -DSYSV -DUSG }%{ZS:-D_SYSV_SOURCE -DSYSV -DUSG }\
%{sposix:-D_POSIX_SOURCE -DPOSIX }%{ZP:-D_POSIX_SOURCE -DPOSIX }\
%{sposix+:-D_POSIX_SOURCE -DPOSIX }\
%{saux:-D_AUX_SOURCE }%{ZA:-D_AUX_SOURCE }\
%{!sbsd:%{!ZB:%{!ssysv:%{!ZS:%{!sposix:%{!ZP:%{!snone:\
-D_BSD_SOURCE -D_SYSV_SOURCE -D_AUX_SOURCE }}}}}}}"

#define LIB_SPEC \
"%{sbsd:-lbsd }%{ZB:-lbsd }\
%{ssysv:-lsvid }%{ZS:-lsvid }\
%{sposix:-lposix }%{ZP:-lposix }%{sposix+:-lposix }\
%{!static:%{smac:-lmac_s -lat -lld -lmr }-lc_s }\
%{static:%{smac:-lmac -lat -lld -lmr }-lc }"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
"%{pg:mcrt0.o%s }%{!pg:%{p:mcrt1.o%s }\
%{!p:%{smac:maccrt1.o%s low.o%s }%{!smac:crt1.o%s }}}\
crt2.o%s "

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtn.o%s "


/*===================================================================*/
/* Compilation environment -- mostly */

/* We provide atexit(), A/UX does not have it */
#define NEED_ATEXIT

/* Generate calls to memcpy, memcmp and memset, as opposed to bcopy, bcmp,
   and bzero */
#define TARGET_MEM_FUNCTIONS

/* Resize standard types */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

/* Every structure or union's size must be a multiple of 2 bytes.  */
#define STRUCTURE_SIZE_BOUNDARY 16

/* Bits needed by collect */

#define OBJECT_FORMAT_COFF
#define MY_ISCOFF(m)	((m) == M68TVMAGIC || \
			 (m) == M68MAGIC || \
			 (m) == MC68TVMAGIC || \
			 (m) == MC68MAGIC || \
			 (m) == M68NSMAGIC)


#ifndef USE_COLLECT2
/* For .ctor/.dtor sections for collecting constructors */
/* We have special start/end files for defining [cd]tor lists */
#define CTOR_LISTS_DEFINED_EXTERNALLY
#endif


/*======================================================================*/
/* Calling convention and library support changes */

/* Define how to generate (in the callee) the output value of a function
   and how to find (in the caller) the value returned by a function.  VALTYPE
   is the data type of the value (as a tree).  If the precise function being
   called is known, FUNC is its FUNCTION_DECL; otherwise, FUNC is 0.
   For A/UX generate the result in d0, a0, or fp0 as appropriate.  */

#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE, FUNC)                                  \
  (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_68881                    \
   ? gen_rtx_REG (TYPE_MODE (VALTYPE), 16)                            \
   : (POINTER_TYPE_P (VALTYPE)		                               \
      ? gen_rtx_REG (TYPE_MODE (VALTYPE), 8)                           \
      : gen_rtx_REG (TYPE_MODE (VALTYPE), 0)))
                    
#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)						\
  gen_rtx_REG ((MODE), ((TARGET_68881 &&				\
			 ((MODE) == SFmode || (MODE) == DFmode)) ? 16 : 0))

/* 1 if N is a possible register number for a function value.
   For A/UX allow d0, a0, or fp0 as return registers, for integral,
   pointer, or floating types, respectively. Reject fp0 if not using a
   68881 coprocessor.  */

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 0 || (N) == 8 || (TARGET_68881 && (N) == 16))

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1

/* For compatibility with the large body of existing code which does not
   always properly declare external functions returning pointer types, the
   A/UX convention is to copy the value returned for pointer functions
   from a0 to d0 in the function epilogue, so that callers that have
   neglected to properly declare the callee can still find the correct return
   value.  */

#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)				\
{									\
  if (current_function_returns_pointer					\
      && ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))	\
    asm_fprintf (FILE, "\t%s %Ra0,%Rd0\n", ASM_MOV_INSN);		\
}

/* How to call the function profiler */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
  asm_fprintf (FILE, "\t%Olea %LLP%d,%Ra0\n\t%Ojbsr %s\n",		\
	       (LABELNO), FUNCTION_PROFILER_SYMBOL)

/* Finalize the trampoline by flushing the insn cache */

#undef FINALIZE_TRAMPOLINE
#define FINALIZE_TRAMPOLINE(TRAMP)					\
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),	\
		     0, VOIDmode, 2, TRAMP, Pmode,			\
		     plus_constant (TRAMP, TRAMPOLINE_SIZE), Pmode);

/* Clear the instruction cache from `beg' to `end'.  This makes an 
   inline system call to SYS_sysm68k.  The arguments are as follows:

	sysm68k(105, addr, scope, cache, len)

   105	  - the subfunction code to clear the cache
   addr	  - the start address for the flush
   scope  - the scope of the flush (see the cpush insn)
   cache  - which cache to flush (see the cpush insn)
   len    - a factor relating to the number of flushes to perform :
   	    len/16 lines, or len/4096 pages.

   While all this is only really relevant to 040's, the system call
   will just return an error (which we ignore) on other systems.  */

#define CLEAR_INSN_CACHE(beg, end)					\
{									\
    unsigned _beg = (unsigned)(beg), _end = (unsigned)(end);		\
    unsigned _len = ((_end / 16) - (_beg / 16) + 1) * 16;		\
    __asm __volatile(							\
	    ASM_MOV_INSN " %1, %-\n\t"     /* nr lines */		\
	    ASM_MOV_INSN " %#3, %-\n\t"	   /* insn+data caches */	\
	    ASM_MOV_INSN " %#1, %-\n\t"	   /* clear lines */		\
	    ASM_MOV_INSN " %0, %-\n\t"	   /* beginning of buffer */	\
	    ASM_MOV_INSN " %#105, %-\n\t"  /* cache sub-function nr */	\
	    ASM_MOV_INSN " %#0, %-\n\t"	   /* dummy return address */	\
	    ASM_MOV_INSN " %#38, %/d0\n\t" /* system call nr */		\
	    "trap %#0\n\t"						\
	    "add%.l %#24, %/sp"						\
	    : /* no outputs */						\
	    : "g"(_beg), "g"(_len)					\
	    : "%d0");							\
}
