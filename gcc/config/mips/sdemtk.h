/* Definitions of target machine for GNU compiler.
   MIPS SDE version, for use with the SDE C library rather than newlib.
   Copyright (C) 2007-2014 Free Software Foundation, Inc.

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

#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_assert ("system=sde");			\
      builtin_assert ("system=posix");			\
      builtin_define ("__SDE_MIPS__");			\
							\
      /* Deprecated: use __mips_isa_rev >= 2.  */	\
      if (ISA_MIPS32R2)					\
        builtin_define ("__mipsr2");			\
							\
      /* Deprecated: use __mips_fpr == 64.  */		\
      if (TARGET_FLOAT64)				\
	builtin_define ("__mipsfp64");			\
							\
      if (TARGET_NO_FLOAT) 				\
	builtin_define ("__NO_FLOAT");			\
      else if (TARGET_SOFT_FLOAT_ABI)			\
	builtin_define ("__SOFT_FLOAT");		\
      else if (TARGET_SINGLE_FLOAT)			\
	builtin_define ("__SINGLE_FLOAT");		\
							\
      if (TARGET_BIG_ENDIAN)				\
        {						\
	  builtin_assert ("endian=big");		\
	  builtin_assert ("cpu=mipseb");		\
	}						\
      else						\
        {						\
	  builtin_assert ("endian=little");		\
	  builtin_assert ("cpu=mipsel");		\
	}						\
    }							\
  while (0)

/* For __clear_cache in libgcc2.c.  */
#ifdef IN_LIBGCC2
extern void mips_sync_icache (void *beg, unsigned long len);
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(beg, end) \
  mips_sync_icache (beg, end - beg)
#endif

/* For mips_cache_flush_func in mips.opt.  */
#undef CACHE_FLUSH_FUNC
#define CACHE_FLUSH_FUNC "mips_sync_icache"

/* For inline code which needs to sync the icache and dcache,
   noting that the SDE library takes arguments (address, size).  */
#undef MIPS_ICACHE_SYNC
#define MIPS_ICACHE_SYNC(ADDR, SIZE)					\
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, mips_cache_flush_func),	\
		     LCT_NORMAL, VOIDmode, 2, ADDR, Pmode,		\
		     SIZE, TYPE_MODE (sizetype))

/* This version of _mcount does not pop 2 words from the stack.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
  {									\
    mips_push_asm_switch (&mips_noat);					\
    /* _mcount treats $2 as the static chain register.  */		\
    if (cfun->static_chain_decl != NULL)				\
      fprintf (FILE, "\tmove\t%s,%s\n", reg_names[2],			\
	       reg_names[STATIC_CHAIN_REGNUM]);				\
    /* MIPS16 code passes saved $ra in $v1 instead of $at.  */		\
    fprintf (FILE, "\tmove\t%s,%s\n",					\
	     reg_names[GP_REG_FIRST + (TARGET_MIPS16 ? 3 : 1)],		\
	     reg_names[RETURN_ADDR_REGNUM]);				\
    fprintf (FILE, "\tjal\t_mcount\n");					\
    mips_pop_asm_switch (&mips_noat);					\
    /* _mcount treats $2 as the static chain register.  */		\
    if (cfun->static_chain_decl != NULL)				\
      fprintf (FILE, "\tmove\t%s,%s\n", reg_names[STATIC_CHAIN_REGNUM],	\
	       reg_names[2]);						\
  }

/* ...nor does the call sequence preserve $31.  */
#undef MIPS_SAVE_REG_FOR_PROFILING_P
#define MIPS_SAVE_REG_FOR_PROFILING_P(REGNO) ((REGNO) == RETURN_ADDR_REGNUM)

/* Compile in support for the -mno-float option.  */
#define TARGET_SUPPORTS_NO_FLOAT 1
