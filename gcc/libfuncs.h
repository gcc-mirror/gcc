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

  LTI_memcpy,
  LTI_memmove,
  LTI_bcopy,
  LTI_memcmp,
  LTI_bcmp,
  LTI_memset,
  LTI_bzero,

  LTI_unwind_resume,
  LTI_eh_personality,
  LTI_setjmp,
  LTI_longjmp,
  LTI_unwind_sjlj_register,
  LTI_unwind_sjlj_unregister,

  LTI_eqhf2,
  LTI_nehf2,
  LTI_gthf2,
  LTI_gehf2,
  LTI_lthf2,
  LTI_lehf2,
  LTI_unordhf2,

  LTI_eqsf2,
  LTI_nesf2,
  LTI_gtsf2,
  LTI_gesf2,
  LTI_ltsf2,
  LTI_lesf2,
  LTI_unordsf2,

  LTI_eqdf2,
  LTI_nedf2,
  LTI_gtdf2,
  LTI_gedf2,
  LTI_ltdf2,
  LTI_ledf2,
  LTI_unorddf2,

  LTI_eqxf2,
  LTI_nexf2,
  LTI_gtxf2,
  LTI_gexf2,
  LTI_ltxf2,
  LTI_lexf2,
  LTI_unordxf2,

  LTI_eqtf2,
  LTI_netf2,
  LTI_gttf2,
  LTI_getf2,
  LTI_lttf2,
  LTI_letf2,
  LTI_unordtf2,

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

  LTI_MAX
};

/* SYMBOL_REF rtx's for the library functions that are called
   implicitly and not via optabs.  */
extern rtx libfunc_table[LTI_MAX];

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

#define memcpy_libfunc	(libfunc_table[LTI_memcpy])
#define memmove_libfunc	(libfunc_table[LTI_memmove])
#define bcopy_libfunc	(libfunc_table[LTI_bcopy])
#define memcmp_libfunc	(libfunc_table[LTI_memcmp])
#define bcmp_libfunc	(libfunc_table[LTI_bcmp])
#define memset_libfunc	(libfunc_table[LTI_memset])
#define bzero_libfunc	(libfunc_table[LTI_bzero])

#define unwind_resume_libfunc	(libfunc_table[LTI_unwind_resume])
#define eh_personality_libfunc	(libfunc_table[LTI_eh_personality])
#define setjmp_libfunc	(libfunc_table[LTI_setjmp])
#define longjmp_libfunc	(libfunc_table[LTI_longjmp])
#define unwind_sjlj_register_libfunc (libfunc_table[LTI_unwind_sjlj_register])
#define unwind_sjlj_unregister_libfunc \
  (libfunc_table[LTI_unwind_sjlj_unregister])

#define eqhf2_libfunc	(libfunc_table[LTI_eqhf2])
#define nehf2_libfunc	(libfunc_table[LTI_nehf2])
#define gthf2_libfunc	(libfunc_table[LTI_gthf2])
#define gehf2_libfunc	(libfunc_table[LTI_gehf2])
#define lthf2_libfunc	(libfunc_table[LTI_lthf2])
#define lehf2_libfunc	(libfunc_table[LTI_lehf2])
#define unordhf2_libfunc	(libfunc_table[LTI_unordhf2])

#define eqsf2_libfunc	(libfunc_table[LTI_eqsf2])
#define nesf2_libfunc	(libfunc_table[LTI_nesf2])
#define gtsf2_libfunc	(libfunc_table[LTI_gtsf2])
#define gesf2_libfunc	(libfunc_table[LTI_gesf2])
#define ltsf2_libfunc	(libfunc_table[LTI_ltsf2])
#define lesf2_libfunc	(libfunc_table[LTI_lesf2])
#define unordsf2_libfunc	(libfunc_table[LTI_unordsf2])

#define eqdf2_libfunc	(libfunc_table[LTI_eqdf2])
#define nedf2_libfunc	(libfunc_table[LTI_nedf2])
#define gtdf2_libfunc	(libfunc_table[LTI_gtdf2])
#define gedf2_libfunc	(libfunc_table[LTI_gedf2])
#define ltdf2_libfunc	(libfunc_table[LTI_ltdf2])
#define ledf2_libfunc	(libfunc_table[LTI_ledf2])
#define unorddf2_libfunc	(libfunc_table[LTI_unorddf2])

#define eqxf2_libfunc	(libfunc_table[LTI_eqxf2])
#define nexf2_libfunc	(libfunc_table[LTI_nexf2])
#define gtxf2_libfunc	(libfunc_table[LTI_gtxf2])
#define gexf2_libfunc	(libfunc_table[LTI_gexf2])
#define ltxf2_libfunc	(libfunc_table[LTI_ltxf2])
#define lexf2_libfunc	(libfunc_table[LTI_lexf2])
#define unordxf2_libfunc	(libfunc_table[LTI_unordxf2])

#define eqtf2_libfunc	(libfunc_table[LTI_eqtf2])
#define netf2_libfunc	(libfunc_table[LTI_netf2])
#define gttf2_libfunc	(libfunc_table[LTI_gttf2])
#define getf2_libfunc	(libfunc_table[LTI_getf2])
#define lttf2_libfunc	(libfunc_table[LTI_lttf2])
#define letf2_libfunc	(libfunc_table[LTI_letf2])
#define unordtf2_libfunc	(libfunc_table[LTI_unordtf2])

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

#endif /* GCC_LIBFUNCS_H */
