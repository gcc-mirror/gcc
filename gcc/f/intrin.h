/* intrin.h -- Public interface for intrin.c
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

*/

#ifndef _H_f_intrin
#define _H_f_intrin

#ifndef FFEINTRIN_DOC
#define FFEINTRIN_DOC 0	/* 1 means intrinsic documentation only (intdoc.c). */
#endif

typedef enum
  {
    FFEINTRIN_familyNONE,	/* Not in any family. */
    FFEINTRIN_familyF77,	/* ANSI FORTRAN 77. */
    FFEINTRIN_familyGNU,	/* GNU Fortran intrinsics. */
    FFEINTRIN_familyF2C,	/* f2c intrinsics. */
    FFEINTRIN_familyF90,	/* Fortran 90. */
    FFEINTRIN_familyF95 = FFEINTRIN_familyF90,
    FFEINTRIN_familyVXT,	/* VAX/VMS FORTRAN. */
    FFEINTRIN_familyMIL,	/* MIL STD 1753 (MVBITS, etc), in mil, vxt, and f90. */
    FFEINTRIN_familyASC,	/* ASCII-related (ACHAR, IACHAR), both f2c and f90. */
    FFEINTRIN_familyFVZ,	/* in both f2c and VAX/VMS FORTRAN. */
    FFEINTRIN_familyF2U,	/* libf2c/libU77 UNIX system intrinsics. */
    FFEINTRIN_familyBADU77,	/* libU77 UNIX system intrinsics with bad form. */
    FFEINTRIN_family
  } ffeintrinFamily;

typedef enum
  {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2) FFEINTRIN_gen ## CODE,
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
    FFEINTRIN_gen
  } ffeintrinGen;

typedef enum
  {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP) FFEINTRIN_spec ## CODE,
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
    FFEINTRIN_spec
  } ffeintrinSpec;

typedef enum
  {
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL) \
    FFEINTRIN_imp ## CODE,
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD) \
    FFEINTRIN_imp ## CODE,
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
    FFEINTRIN_imp
  } ffeintrinImp;

#if !FFEINTRIN_DOC

#include "bld.h"
#include "info.h"

ffeinfoBasictype ffeintrin_basictype (ffeintrinSpec spec);
ffeintrinFamily ffeintrin_family (ffeintrinSpec spec);
void ffeintrin_fulfill_generic (ffebld *expr, ffeinfo *info, ffelexToken t);
void ffeintrin_fulfill_specific (ffebld *expr, ffeinfo *info,
				 bool *check_intrin, ffelexToken t);
#if FFECOM_targetCURRENT == FFECOM_targetGCC
ffecomGfrt ffeintrin_gfrt_direct (ffeintrinImp imp);
ffecomGfrt ffeintrin_gfrt_indirect (ffeintrinImp imp);
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
void ffeintrin_init_0 (void);
#define ffeintrin_init_1()
#define ffeintrin_init_2()
#define ffeintrin_init_3()
#define ffeintrin_init_4()
bool ffeintrin_is_actualarg (ffeintrinSpec spec);
bool ffeintrin_is_intrinsic (const char *name, ffelexToken t, bool explicit,
			     ffeintrinGen *gen, ffeintrinSpec *spec,
			     ffeintrinImp *imp);
bool ffeintrin_is_standard (ffeintrinGen gen, ffeintrinSpec spec);
ffeinfoKindtype ffeintrin_kindtype (ffeintrinSpec spec);
const char *ffeintrin_name_generic (ffeintrinGen gen);
const char *ffeintrin_name_implementation (ffeintrinImp imp);
const char *ffeintrin_name_specific (ffeintrinSpec spec);
ffeIntrinsicState ffeintrin_state_family (ffeintrinFamily family);
#define ffeintrin_terminate_0()
#define ffeintrin_terminate_1()
#define ffeintrin_terminate_2()
#define ffeintrin_terminate_3()
#define ffeintrin_terminate_4()

#endif	/* !FFEINTRIN_DOC */

/* End of #include file. */

#endif
