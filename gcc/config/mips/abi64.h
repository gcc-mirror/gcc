/* Definitions of target machine for GNU compiler.  64 bit ABI support.
   Copyright (C) 1994, 1995, 1996, 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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

#undef SUBTARGET_TARGET_OPTIONS
#define SUBTARGET_TARGET_OPTIONS \
  { "abi=", &mips_abi_string,						\
      "Specify ABI to use"},

#undef STACK_BOUNDARY
#define STACK_BOUNDARY \
  ((mips_abi == ABI_32 || mips_abi == ABI_O64 || mips_abi == ABI_EABI) \
   ? 64 : 128)

#undef MIPS_STACK_ALIGN
#define MIPS_STACK_ALIGN(LOC)						\
  ((mips_abi == ABI_32 || mips_abi == ABI_O64 || mips_abi == ABI_EABI)	\
   ? ((LOC) + 7) & ~7							\
   : ((LOC) + 15) & ~15)

#undef GP_ARG_LAST
#define GP_ARG_LAST  ((mips_abi == ABI_32 || mips_abi == ABI_O64)	\
		      ? GP_REG_FIRST + 7 : GP_REG_FIRST + 11)
#undef FP_ARG_LAST
#define FP_ARG_LAST  ((mips_abi == ABI_32 || mips_abi == ABI_O64)	\
		      ? FP_REG_FIRST + 15 : FP_REG_FIRST + 19)

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
  if (mips_abi == ABI_N32 || mips_abi == ABI_MEABI)  			\
    {									\
      int regno;							\
      for (regno = FP_REG_FIRST + 21; regno <= FP_REG_FIRST + 31; regno+=2) \
	call_used_regs[regno] = 1;					\
    }									\
}

#undef MAX_ARGS_IN_REGISTERS
#define MAX_ARGS_IN_REGISTERS ((mips_abi == ABI_32 || mips_abi == ABI_O64) \
			       ? 4 : 8)

#undef REG_PARM_STACK_SPACE
#define REG_PARM_STACK_SPACE(FNDECL) 					 \
  ((mips_abi == ABI_32 || mips_abi == ABI_O64)				 \
   ? (MAX_ARGS_IN_REGISTERS*UNITS_PER_WORD) - FIRST_PARM_OFFSET (FNDECL) \
   : 0)

#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (! BYTES_BIG_ENDIAN							\
   ? upward								\
   : (((MODE) == BLKmode						\
       ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST		\
	  && int_size_in_bytes (TYPE) < (PARM_BOUNDARY / BITS_PER_UNIT))\
       : (GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY			\
	  && (mips_abi == ABI_32					\
	      || mips_abi == ABI_O64					\
	      || mips_abi == ABI_EABI					\
	      || GET_MODE_CLASS (MODE) == MODE_INT)))			\
      ? downward : upward))

/* Modified version of the macro in expr.h.  */
#define MUST_PASS_IN_STACK(MODE,TYPE)			\
  ((TYPE) != 0						\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
       || TREE_ADDRESSABLE (TYPE)			\
       || ((MODE) == BLKmode 				\
	   && mips_abi != ABI_32 && mips_abi != ABI_O64 \
	   && ! ((TYPE) != 0 && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST \
		 && 0 == (int_size_in_bytes (TYPE)	\
			  % (PARM_BOUNDARY / BITS_PER_UNIT))) \
	   && (FUNCTION_ARG_PADDING (MODE, TYPE)	\
	       == (BYTES_BIG_ENDIAN ? upward : downward)))))

#define STRICT_ARGUMENT_NAMING (mips_abi != ABI_32 && mips_abi != ABI_O64)

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of the
   argument itself.  The pointer is passed in whatever way is appropriate
   for passing a pointer to that type.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  function_arg_pass_by_reference (&CUM, MODE, TYPE, NAMED)

/* A C expression that indicates when it is the called function's
   responsibility to make a copy of arguments passed by invisible
   reference.  Normally, the caller makes a copy and passes the
   address of the copy to the routine being called.  When
   FUNCTION_ARG_CALLEE_COPIES is defined and is nonzero, the caller
   does not make a copy.  Instead, it passes a pointer to the "live"
   value.  The called function must not modify this value.  If it can
   be determined that the value won't be modified, it need not make a
   copy; otherwise a copy must be made.  */
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED)		\
  (mips_abi == ABI_EABI && (NAMED)					\
   && FUNCTION_ARG_PASS_BY_REFERENCE (CUM, MODE, TYPE, NAMED))

/* ??? Unimplemented stuff follows.  */

/* ??? Add support for 16 byte/128 bit long doubles here when
   mips_abi != ABI32.  */

/* ??? Make main return zero if user did not specify return value.  */

/* ??? Add support for .interfaces section, so as to get linker warnings
   when stdarg functions called without prototype in scope?  */

/* ??? Could optimize structure passing by putting the right register rtx
   into the field decl, so that if we use the field, we can take the value from
   a register instead of from memory.  */
