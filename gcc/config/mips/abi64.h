/* Definitions of target machine for GNU compiler.  64 bit ABI support.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

/* Macros to implement the 64 bit ABI.  This file is meant to be included
   after mips.h.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_ABICALLS|MASK_FLOAT64|MASK_64BIT

/* For Irix 6, -mips3 implies TARGET_LONG64.  */
#undef TARGET_LONG64
#define TARGET_LONG64		(mips_abi == ABI_64)

#undef SUBTARGET_TARGET_OPTIONS
#define SUBTARGET_TARGET_OPTIONS\
  { "abi=", &mips_abi_string	},

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Dunix -Dmips -Dsgi -Dhost_mips -DMIPSEB -D_MIPSEB -DSYSTYPE_SVR4 \
  -D_SVR4_SOURCE -D_MODERN_C -D__DSO__ \
  -Asystem(unix) -Asystem(svr4) -Acpu(mips) -Amachine(sgi)"

/* We must make -mips3 do what -mlong64 used to do.  */
/* ??? If no mipsX option given, but a mabi=X option is, then should set
   _MIPS_ISA based on the mabi=X option.  */
/* ??? If no mabi=X option give, but a mipsX option is, then should set
   _MIPS_SIM based on the mipsX option.  */
/* ??? Same for _MIPS_SZINT.  */
/* ??? Same for _MIPS_SZPTR.  */
/* ??? Same for __SIZE_TYPE and __PTRDIFF_TYPE.  */
#undef CPP_SPEC
#define CPP_SPEC "\
%{!ansi:-D__EXTENSIONS__ -D_SGI_SOURCE -D_LONGLONG} \
%{.cc:	-D_LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D_LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D_LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D_LANGUAGE_OBJECTIVE_C -D_LANGUAGE_C} \
%{.S:	-D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.s:	-D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{!.S:%{!.s: %{!.cc: %{!.cxx: %{!.C: %{!.m: -D_LANGUAGE_C %{!ansi:-DLANGUAGE_C}}}}}}}\
%{mfp32: -D_MIPS_FPSET=16}%{!mfp32: -D_MIPS_FPSET=32} \
%{mips1: -D_MIPS_ISA=_MIPS_ISA_MIPS1} \
%{mips2: -D_MIPS_ISA=_MIPS_ISA_MIPS2} \
%{mips3: -D_MIPS_ISA=_MIPS_ISA_MIPS3} \
%{mips4: -D_MIPS_ISA=_MIPS_ISA_MIPS4} \
%{!mips1: %{!mips2: %{!mips3: %{!mips4: -D_MIPS_ISA=_MIPS_ISA_MIPS3}}}} \
%{mabi=32: -D_MIPS_SIM=_MIPS_SIM_ABI32}	\
%{mabi=n32: -D_ABIN32=2 -D_MIPS_SIM=_ABIN32} \
%{mabi=64: -D_ABI64=3 -D_MIPS_SIM=_ABI64} \
%{!mabi=32: %{!mabi=n32: %{!mabi=64: -D_ABIN32=2 -D_MIPS_SIM=_ABIN32}}}	\
%{!mint64: -D_MIPS_SZINT=32}%{mint64: -D_MIPS_SZINT=64} \
%{mabi=32: -D_MIPS_SZLONG=32} \
%{mabi=n32: -D_MIPS_SZLONG=32} \
%{mabi=64: -D_MIPS_SZLONG=64} \
%{!mabi=32: %{!mabi=n32: %{!mabi=64: -D_MIPS_SZLONG=32}}} \
%{mabi=32: -D_MIPS_SZPTR=32} \
%{mabi=n32: -D_MIPS_SZPTR=32} \
%{mabi=64: -D_MIPS_SZPTR=64} \
%{!mabi=32: %{!mabi=n32: %{!mabi=64: -D_MIPS_SZPTR=32}}} \
%{!mips1:%{!mips2: -D_COMPILER_VERSION=601}}		\
%{mabi=32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=n32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=64: -D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
%{!mabi=32:%{!mabi=n32: %{!mabi=64: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}}} \
%{mips3: -U__mips -D__mips=3} \
%{mips4: -U__mips -D__mips=4} \
%{!mips1:%{!mips2:%{!mips3:%{!mips4: -U__mips -D__mips=3}}}} \
%{mgp32: -U__mips64}%{mgp64: -D__mips64} \
%{mabi=32: -U__mips64} \
%{mabi=n32: -D__mips64} \
%{mabi=64: -D__mips64} \
%{!mabi=32: %{!mabi=n32: %{!mabi=64: -D__mips64}}} \
%{msingle-float:%{!msoft-float:-D__mips_single_float}} \
%{m4650:%{!msoft-float:-D__mips_single_float}} \
%{EB:-UMIPSEL -U_MIPSEL -U__MIPSEL -U__MIPSEL__ -D_MIPSEB -D__MIPSEB -D__MIPSEB__ %{!ansi:-DMIPSEB}} \
%{EL:-UMIPSEB -U_MIPSEB -U__MIPSEB -U__MIPSEB__ -D_MIPSEL -D__MIPSEL -D__MIPSEL__ %{!ansi:-DMIPSEL}}"

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY	32

#undef STACK_BOUNDARY
#define STACK_BOUNDARY (mips_abi == ABI_32 ? 64 : 128)

#undef MIPS_STACK_ALIGN
#define MIPS_STACK_ALIGN(LOC) \
  (mips_abi == ABI_32 ? ((LOC)+7) & ~7 : ((LOC)+15) & ~15)

#undef GP_ARG_LAST
#define GP_ARG_LAST  (mips_abi == ABI_32 ? GP_REG_FIRST + 7 : GP_REG_FIRST + 11)
#undef FP_ARG_LAST
#define FP_ARG_LAST  (mips_abi == ABI_32 ? FP_REG_FIRST + 15 : FP_REG_FIRST + 19)

#undef SUBTARGET_CONDITIONAL_REGISTER_USAGE
#define SUBTARGET_CONDITIONAL_REGISTER_USAGE \
{									\
  /* fp20-23 are now caller saved.  */					\
  if (mips_abi == ABI_64)						\
    {									\
      int regno;							\
      for (regno = FP_REG_FIRST + 20; regno < FP_REG_FIRST + 24; regno++) \
	call_used_regs[regno] = 1;					\
    }									\
  /* odd registers from fp21 to fp31 are now caller saved.  */		\
  if (mips_abi == ABI_N32)						\
    {									\
      int regno;							\
      for (regno = FP_REG_FIRST + 21; regno <= FP_REG_FIRST + 31; regno+=2) \
	call_used_regs[regno] = 1;					\
    }									\
}

#undef MAX_ARGS_IN_REGISTERS
#define MAX_ARGS_IN_REGISTERS	(mips_abi == ABI_32 ? 4 : 8)

#undef REG_PARM_STACK_SPACE
#if 0
/* ??? This is necessary in order for the ABI_32 support to work.  However,
   expr.c (emit_push_insn) has no support for a REG_PARM_STACK_SPACE
   definition that returns zero.  That would have to be fixed before this
   can be enabled.  */
#define REG_PARM_STACK_SPACE(FNDECL) 					 \
  (mips_abi == ABI_32							 \
   ? (MAX_ARGS_IN_REGISTERS*UNITS_PER_WORD) - FIRST_PARM_OFFSET (FNDECL) \
   : 0)
#endif

#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (! BYTES_BIG_ENDIAN							\
   ? upward								\
   : (((MODE) == BLKmode						\
       ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST		\
	  && int_size_in_bytes (TYPE) < (PARM_BOUNDARY / BITS_PER_UNIT))\
       : (GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY			\
	  && (mips_abi == ABI_32 || GET_MODE_CLASS (MODE) == MODE_INT)))\
      ? downward : upward))

#undef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE)	\
  (mips_abi == ABI_32							\
   ? TYPE_MODE (TYPE) == BLKmode : int_size_in_bytes (TYPE) > 16)

extern struct rtx_def *mips_function_value ();
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE, FUNC)	mips_function_value (VALTYPE, FUNC)

/* For varargs, we must save the current argument, because it is the fake
   argument va_alist, and will need to be converted to the real argument.
   For stdarg, we do not need to save the current argument, because it
   is a real argument.  */
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if (mips_abi != ABI_32						\
      && ((CUM).arg_words						\
	  < (MAX_ARGS_IN_REGISTERS - ! current_function_varargs)))	\
    {									\
      PRETEND_SIZE							\
	= (MAX_ARGS_IN_REGISTERS - (CUM).arg_words			\
	   - ! current_function_varargs) * UNITS_PER_WORD;		\
									\
      if (! (NO_RTL))							\
	{								\
	  rtx mem = gen_rtx (MEM, BLKmode, virtual_incoming_args_rtx);	\
	  /* va_arg is an array access in this case, which causes it to \
	     get MEM_IN_STRUCT_P set.  We must set it here so that the	\
	     insn scheduler won't assume that these stores can't 	\
	     possibly overlap with the va_arg loads.  */		\
	  if (BYTES_BIG_ENDIAN)						\
	    MEM_IN_STRUCT_P (mem) = 1;					\
	  move_block_from_reg						\
	    ((CUM).arg_words + GP_ARG_FIRST + ! current_function_varargs, \
	     mem,							\
	     (MAX_ARGS_IN_REGISTERS - (CUM).arg_words			\
	      - ! current_function_varargs),				\
	     PRETEND_SIZE);						\
	}								\
    }									\
}

/* ??? Should disable for mips_abi == ABI32.  */
#define STRICT_ARGUMENT_NAMING

/* ??? Unimplemented stuff follows.  */

/* ??? Add support for 16 byte/128 bit long doubles here when
   mips_abi != ABI32.  */

/* ??? Make main return zero if user did not specify return value.  */

/* ??? Add support for .interfaces section, so as to get linker warnings
   when stdarg functions called without prototype in scope?  */

/* ??? Could optimize structure passing by putting the right register rtx
   into the field decl, so that if we use the field, we can take the value from
   a register instead of from memory.  */

