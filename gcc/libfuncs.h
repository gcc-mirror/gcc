/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_LIBFUNCS_H
#define GCC_LIBFUNCS_H

/* Enumeration of indexes into libfunc_table.  */
enum libfunc_index
{
  LTI_extendsfdf2,
  LTI_extendsfxf2,
  LTI_extendsftf2,
  LTI_extenddfxf2,
  LTI_extenddftf2,

  LTI_truncdfsf2,
  LTI_truncxfsf2,
  LTI_trunctfsf2,
  LTI_truncxfdf2,
  LTI_trunctfdf2,

  LTI_abort,
  LTI_memcpy,
  LTI_memmove,
  LTI_bcopy,
  LTI_memcmp,
  LTI_bcmp,
  LTI_memset,
  LTI_bzero,
  LTI_setbits,

  LTI_unwind_resume,
  LTI_eh_personality,
  LTI_setjmp,
  LTI_longjmp,
  LTI_unwind_sjlj_register,
  LTI_unwind_sjlj_unregister,

  LTI_floatsisf,
  LTI_floatdisf,
  LTI_floattisf,

  LTI_floatsidf,
  LTI_floatdidf,
  LTI_floattidf,

  LTI_floatsixf,
  LTI_floatdixf,
  LTI_floattixf,

  LTI_floatsitf,
  LTI_floatditf,
  LTI_floattitf,

  LTI_fixsfsi,
  LTI_fixsfdi,
  LTI_fixsfti,

  LTI_fixdfsi,
  LTI_fixdfdi,
  LTI_fixdfti,

  LTI_fixxfsi,
  LTI_fixxfdi,
  LTI_fixxfti,

  LTI_fixtfsi,
  LTI_fixtfdi,
  LTI_fixtfti,

  LTI_fixunssfsi,
  LTI_fixunssfdi,
  LTI_fixunssfti,

  LTI_fixunsdfsi,
  LTI_fixunsdfdi,
  LTI_fixunsdfti,

  LTI_fixunsxfsi,
  LTI_fixunsxfdi,
  LTI_fixunsxfti,

  LTI_fixunstfsi,
  LTI_fixunstfdi,
  LTI_fixunstfti,

  LTI_profile_function_entry,
  LTI_profile_function_exit,

  LTI_gcov_flush,
  LTI_gcov_init,

  LTI_MAX
};

/* SYMBOL_REF rtx's for the library functions that are called
   implicitly and not via optabs.  */
extern GTY(()) rtx libfunc_table[LTI_MAX];

/* Accessor macros for libfunc_table.  */
#define extendsfdf2_libfunc	(libfunc_table[LTI_extendsfdf2])
#define extendsfxf2_libfunc	(libfunc_table[LTI_extendsfxf2])
#define extendsftf2_libfunc	(libfunc_table[LTI_extendsftf2])
#define extenddfxf2_libfunc	(libfunc_table[LTI_extenddfxf2])
#define extenddftf2_libfunc	(libfunc_table[LTI_extenddftf2])

#define truncdfsf2_libfunc	(libfunc_table[LTI_truncdfsf2])
#define truncxfsf2_libfunc	(libfunc_table[LTI_truncxfsf2])
#define trunctfsf2_libfunc	(libfunc_table[LTI_trunctfsf2])
#define truncxfdf2_libfunc	(libfunc_table[LTI_truncxfdf2])
#define trunctfdf2_libfunc	(libfunc_table[LTI_trunctfdf2])

#define abort_libfunc	(libfunc_table[LTI_abort])
#define memcpy_libfunc	(libfunc_table[LTI_memcpy])
#define memmove_libfunc	(libfunc_table[LTI_memmove])
#define bcopy_libfunc	(libfunc_table[LTI_bcopy])
#define memcmp_libfunc	(libfunc_table[LTI_memcmp])
#define bcmp_libfunc	(libfunc_table[LTI_bcmp])
#define memset_libfunc	(libfunc_table[LTI_memset])
#define bzero_libfunc	(libfunc_table[LTI_bzero])
#define setbits_libfunc	(libfunc_table[LTI_setbits])

#define unwind_resume_libfunc	(libfunc_table[LTI_unwind_resume])
#define eh_personality_libfunc	(libfunc_table[LTI_eh_personality])
#define setjmp_libfunc	(libfunc_table[LTI_setjmp])
#define longjmp_libfunc	(libfunc_table[LTI_longjmp])
#define unwind_sjlj_register_libfunc (libfunc_table[LTI_unwind_sjlj_register])
#define unwind_sjlj_unregister_libfunc \
  (libfunc_table[LTI_unwind_sjlj_unregister])

#define floatsisf_libfunc	(libfunc_table[LTI_floatsisf])
#define floatdisf_libfunc	(libfunc_table[LTI_floatdisf])
#define floattisf_libfunc	(libfunc_table[LTI_floattisf])

#define floatsidf_libfunc	(libfunc_table[LTI_floatsidf])
#define floatdidf_libfunc	(libfunc_table[LTI_floatdidf])
#define floattidf_libfunc	(libfunc_table[LTI_floattidf])

#define floatsixf_libfunc	(libfunc_table[LTI_floatsixf])
#define floatdixf_libfunc	(libfunc_table[LTI_floatdixf])
#define floattixf_libfunc	(libfunc_table[LTI_floattixf])

#define floatsitf_libfunc	(libfunc_table[LTI_floatsitf])
#define floatditf_libfunc	(libfunc_table[LTI_floatditf])
#define floattitf_libfunc	(libfunc_table[LTI_floattitf])

#define fixsfsi_libfunc	(libfunc_table[LTI_fixsfsi])
#define fixsfdi_libfunc	(libfunc_table[LTI_fixsfdi])
#define fixsfti_libfunc	(libfunc_table[LTI_fixsfti])

#define fixdfsi_libfunc	(libfunc_table[LTI_fixdfsi])
#define fixdfdi_libfunc	(libfunc_table[LTI_fixdfdi])
#define fixdfti_libfunc	(libfunc_table[LTI_fixdfti])

#define fixxfsi_libfunc	(libfunc_table[LTI_fixxfsi])
#define fixxfdi_libfunc	(libfunc_table[LTI_fixxfdi])
#define fixxfti_libfunc	(libfunc_table[LTI_fixxfti])

#define fixtfsi_libfunc	(libfunc_table[LTI_fixtfsi])
#define fixtfdi_libfunc	(libfunc_table[LTI_fixtfdi])
#define fixtfti_libfunc	(libfunc_table[LTI_fixtfti])

#define fixunssfsi_libfunc	(libfunc_table[LTI_fixunssfsi])
#define fixunssfdi_libfunc	(libfunc_table[LTI_fixunssfdi])
#define fixunssfti_libfunc	(libfunc_table[LTI_fixunssfti])

#define fixunsdfsi_libfunc	(libfunc_table[LTI_fixunsdfsi])
#define fixunsdfdi_libfunc	(libfunc_table[LTI_fixunsdfdi])
#define fixunsdfti_libfunc	(libfunc_table[LTI_fixunsdfti])

#define fixunsxfsi_libfunc	(libfunc_table[LTI_fixunsxfsi])
#define fixunsxfdi_libfunc	(libfunc_table[LTI_fixunsxfdi])
#define fixunsxfti_libfunc	(libfunc_table[LTI_fixunsxfti])

#define fixunstfsi_libfunc	(libfunc_table[LTI_fixunstfsi])
#define fixunstfdi_libfunc	(libfunc_table[LTI_fixunstfdi])
#define fixunstfti_libfunc	(libfunc_table[LTI_fixunstfti])

#define profile_function_entry_libfunc	(libfunc_table[LTI_profile_function_entry])
#define profile_function_exit_libfunc	(libfunc_table[LTI_profile_function_exit])

#define gcov_flush_libfunc	(libfunc_table[LTI_gcov_flush])
#define gcov_init_libfunc	(libfunc_table[LTI_gcov_init])

#endif /* GCC_LIBFUNCS_H */
