/* Definitions of target machine for GNU compiler.  64 bit ABI support.
   Copyright (C) 1994, 1995, 1996, 1998, 1999 Free Software Foundation, Inc.

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
      "Speciy ABI to use"},

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
  if (mips_abi == ABI_N32)						\
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

#undef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE)						\
  ((mips_abi == ABI_32 || mips_abi == ABI_O64)				\
   ? TYPE_MODE (TYPE) == BLKmode					\
   : (int_size_in_bytes (TYPE)						\
      > (mips_abi == ABI_EABI ? 2 * UNITS_PER_WORD : 16)))

#ifdef ANSI_PROTOTYPES
union tree_node;
#endif
extern struct rtx_def *mips_function_value PARAMS ((union tree_node *, union tree_node *));
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE, FUNC)	mips_function_value (VALTYPE, FUNC)

/* For varargs, we must save the current argument, because it is the fake
   argument va_alist, and will need to be converted to the real argument.
   For stdarg, we do not need to save the current argument, because it
   is a real argument.  */
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ unsigned int mips_off							\
    = (! current_function_varargs) && (! (CUM).last_arg_fp);		\
    unsigned int mips_fp_off						\
    = (! current_function_varargs) && ((CUM).last_arg_fp); 		\
  if (((mips_abi != ABI_32 && mips_abi != ABI_O64)			\
       && (CUM).arg_words < MAX_ARGS_IN_REGISTERS - mips_off)		\
      || (mips_abi == ABI_EABI						\
	  && ! TARGET_SOFT_FLOAT					\
	  && (CUM).fp_arg_words < MAX_ARGS_IN_REGISTERS - mips_fp_off))	\
    {									\
      int mips_save_gp_regs						\
        = MAX_ARGS_IN_REGISTERS - (CUM).arg_words - mips_off;		\
      int mips_save_fp_regs						\
        = (mips_abi != ABI_EABI ? 0					\
	   : MAX_ARGS_IN_REGISTERS - (CUM).fp_arg_words - mips_fp_off);	\
									\
      if (mips_save_gp_regs < 0)					\
	mips_save_gp_regs = 0;						\
      if (mips_save_fp_regs < 0)					\
	mips_save_fp_regs = 0;						\
      PRETEND_SIZE = ((mips_save_gp_regs * UNITS_PER_WORD)		\
		      + (mips_save_fp_regs * UNITS_PER_FPREG));		\
									\
      if (! (NO_RTL))							\
	{								\
	  if ((CUM).arg_words < MAX_ARGS_IN_REGISTERS - mips_off)	\
	    {								\
	      rtx ptr, mem;						\
	      if (mips_abi != ABI_EABI)					\
		ptr = virtual_incoming_args_rtx;			\
	      else							\
		ptr = plus_constant (virtual_incoming_args_rtx,		\
				     - (mips_save_gp_regs		\
					* UNITS_PER_WORD));		\
	      mem = gen_rtx_MEM (BLKmode, ptr);			\
	      /* va_arg is an array access in this case, which causes	\
		 it to get MEM_IN_STRUCT_P set.  We must set it here	\
		 so that the insn scheduler won't assume that these	\
		 stores can't possibly overlap with the va_arg loads.  */ \
	      if (mips_abi != ABI_EABI && BYTES_BIG_ENDIAN)		\
	        MEM_SET_IN_STRUCT_P (mem, 1);				\
	      move_block_from_reg					\
		((CUM).arg_words + GP_ARG_FIRST + mips_off,		\
		 mem,							\
		 mips_save_gp_regs,					\
		 mips_save_gp_regs * UNITS_PER_WORD);			\
	    }								\
	  if (mips_abi == ABI_EABI					\
	      && ! TARGET_SOFT_FLOAT					\
	      && (CUM).fp_arg_words < MAX_ARGS_IN_REGISTERS - mips_fp_off) \
	    {								\
	      enum machine_mode mode = TARGET_SINGLE_FLOAT ? SFmode : DFmode; \
	      int size = GET_MODE_SIZE (mode);				\
	      int off;							\
	      int i;							\
	      /* We can't use move_block_from_reg, because it will use	\
                 the wrong mode.  */					\
	      off = - (mips_save_gp_regs * UNITS_PER_WORD);		\
	      if (! TARGET_SINGLE_FLOAT)				\
	        off &= ~ 7;						\
	      if (! TARGET_FLOAT64 || TARGET_SINGLE_FLOAT)		\
		off -= (mips_save_fp_regs / 2) * size;			\
	      else							\
		off -= mips_save_fp_regs * size;			\
	      for (i = 0; i < mips_save_fp_regs; i++)			\
		{							\
		  rtx tem =						\
		    gen_rtx_MEM (mode,					\
				 plus_constant (virtual_incoming_args_rtx, \
						off));			\
		  emit_move_insn (tem,					\
				  gen_rtx_REG (mode,			\
					       ((CUM).fp_arg_words	\
						+ FP_ARG_FIRST		\
						+ i			\
						+ mips_fp_off)));	\
		  off += size;						\
		  if (! TARGET_FLOAT64 || TARGET_SINGLE_FLOAT)		\
		    ++i;						\
		}							\
	    }								\
	}								\
    }									\
}

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
   copy; otherwise a copy must be made.

   ??? The MIPS EABI says that the caller should copy in ``K&R mode.''
   I don't know how to detect that here, since flag_traditional is not
   a back end flag.  */
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED)		\
  (mips_abi == ABI_EABI && (NAMED)					\
   && FUNCTION_ARG_PASS_BY_REFERENCE (CUM, MODE, TYPE, NAMED))

/* Define LONG_MAX correctly for all users.  We need to handle 32 bit EABI,
   64 bit EABI, N32, and N64 as possible defaults.  The checks performed here
   are the same as the checks in override_options in mips.c that determines
   whether MASK_LONG64 will be set.

   This does not handle inappropriate options or ununusal option
   combinations.  */

#undef LONG_MAX_SPEC
#if ((MIPS_ABI_DEFAULT == ABI_64) || ((MIPS_ABI_DEFAULT == ABI_EABI) && ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_64BIT)))
#define LONG_MAX_SPEC \
  "%{!mabi=32:%{!mabi=n32:%{!mlong32:%{!mgp32:%{!mips1:%{!mips2:-D__LONG_MAX__=9223372036854775807L}}}}}}"
#else
#define LONG_MAX_SPEC \
  "%{mabi=64:-D__LONG_MAX__=9223372036854775807L} \
   %{mlong64:-D__LONG_MAX__=9223372036854775807L} \
   %{mgp64:-D__LONG_MAX__=9223372036854775807L}"
#endif

/* ??? Unimplemented stuff follows.  */

/* ??? Add support for 16 byte/128 bit long doubles here when
   mips_abi != ABI32.  */

/* ??? Make main return zero if user did not specify return value.  */

/* ??? Add support for .interfaces section, so as to get linker warnings
   when stdarg functions called without prototype in scope?  */

/* ??? Could optimize structure passing by putting the right register rtx
   into the field decl, so that if we use the field, we can take the value from
   a register instead of from memory.  */



