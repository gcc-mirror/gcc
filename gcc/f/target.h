/* target.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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

   Owning Modules:
      target.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_target
#define _H_f_target

#ifdef FFE_STANDALONE
#define HOST_WIDE_INT long
#else
#ifndef TREE_CODE
#include "tree.j"
#endif
#endif

/* For now, g77 requires the ability to determine the exact bit pattern
   of a float on the target machine.  (Hopefully this will be changed
   soon).  Make sure we can do this.  */

#if !defined (REAL_ARITHMETIC) \
  && ((TARGET_FLOAT_FORMAT != HOST_FLOAT_FORMAT) \
      || (FLOAT_WORDS_BIG_ENDIAN != HOST_FLOAT_WORDS_BIG_ENDIAN))
#error "g77 requires ability to access exact FP representation of target machine"
#endif

/* Simple definitions and enumerations. */

#define FFETARGET_charactersizeNONE (-1)
#ifndef FFETARGET_charactersizeMAXIMUM
#define FFETARGET_charactersizeMAXIMUM 2147483647
#endif

#ifndef FFETARGET_defaultIS_90
#define FFETARGET_defaultIS_90 0
#endif
#ifndef FFETARGET_defaultIS_AUTOMATIC
#define FFETARGET_defaultIS_AUTOMATIC 1
#endif
#ifndef FFETARGET_defaultIS_BACKSLASH
#define FFETARGET_defaultIS_BACKSLASH 1
#endif
#ifndef FFETARGET_defaultIS_INIT_LOCAL_ZERO
#define FFETARGET_defaultIS_INIT_LOCAL_ZERO 0
#endif
#ifndef FFETARGET_defaultIS_DOLLAR_OK
#define FFETARGET_defaultIS_DOLLAR_OK 0
#endif
#ifndef FFETARGET_defaultIS_F2C
#define FFETARGET_defaultIS_F2C 1
#endif
#ifndef FFETARGET_defaultIS_F2C_LIBRARY
#define FFETARGET_defaultIS_F2C_LIBRARY 1
#endif
#ifndef FFETARGET_defaultIS_FREE_FORM
#define FFETARGET_defaultIS_FREE_FORM 0
#endif
#ifndef FFETARGET_defaultIS_PEDANTIC
#define FFETARGET_defaultIS_PEDANTIC 0
#endif
#ifndef FFETARGET_defaultCASE_INTRIN
#define FFETARGET_defaultCASE_INTRIN FFE_caseLOWER
#endif
#ifndef FFETARGET_defaultCASE_MATCH
#define FFETARGET_defaultCASE_MATCH FFE_caseLOWER
#endif
#ifndef FFETARGET_defaultCASE_SOURCE
#define FFETARGET_defaultCASE_SOURCE FFE_caseLOWER
#endif
#ifndef FFETARGET_defaultCASE_SYMBOL
#define FFETARGET_defaultCASE_SYMBOL FFE_caseNONE
#endif

#ifndef FFETARGET_defaultFIXED_LINE_LENGTH
#define FFETARGET_defaultFIXED_LINE_LENGTH 72
#endif

/* 1 if external Fortran names ("FOO" in SUBROUTINE FOO, COMMON /FOO/,
   and even enforced/default-for-unnamed PROGRAM, blank-COMMON, and
   BLOCK DATA names, but not names of library functions implementing
   intrinsics or names of local/internal variables) should have an
   underscore appended (for compatibility with existing systems).  */

#ifndef FFETARGET_defaultEXTERNAL_UNDERSCORED
#define FFETARGET_defaultEXTERNAL_UNDERSCORED 1
#endif

/* 1 if external Fortran names with underscores already in them should
   have an extra underscore appended (in addition to the one they
   might already have appened if FFETARGET_defaultEXTERNAL_UNDERSCORED). */

#ifndef FFETARGET_defaultUNDERSCORED_EXTERNAL_UNDERSCORED
#define FFETARGET_defaultUNDERSCORED_EXTERNAL_UNDERSCORED 1
#endif

/* If FFETARGET_defaultEXTERNAL_UNDERSCORED is 0, the following definitions
   might also need to be overridden to make g77 objects compatible with
   f2c+gcc objects.  Although I don't think the unnamed BLOCK DATA one
   is an issue at all.  Of course, on some systems it isn't f2c
   compatibility that is the issue -- maybe compatibility with some
   other compiler(s).  I don't know what to recommend for systems where
   there is no existing Fortran compiler -- I suppose porting f2c and
   pretending it's the existing one is best for now.  */

/* 1 if the "FOO" in "PROGRAM FOO" should be overridden and a particular
   name imposed in place of it in the actual code (normally the case,
   because the library's main entry point on most systems calls the main
   function by a particular name).  Someday g77 might do the f2c trick
   of also outputting a "FOO" procedure that just calls the main procedure,
   but that'll wait until somebody shows why it is needed.  */

#ifndef FFETARGET_isENFORCED_MAIN
#define FFETARGET_isENFORCED_MAIN 1
#endif

/* The enforced name of the main program if ENFORCED_MAIN is 1.  */

#ifndef FFETARGET_nameENFORCED_MAIN_NAME
#define FFETARGET_nameENFORCED_MAIN_NAME "MAIN__"
#endif

/* The name used for an unnamed main program if ENFORCED_MAIN is 0.  */

#ifndef FFETARGET_nameUNNAMED_MAIN
#define FFETARGET_nameUNNAMED_MAIN "MAIN__"
#endif

/* The name used for an unnamed block data program.  */

#ifndef FFETARGET_nameUNNAMED_BLOCK_DATA
#define FFETARGET_nameUNNAMED_BLOCK_DATA "_BLOCK_DATA__"
#endif

/* The name used for blank common.  */

#ifndef FFETARGET_nameBLANK_COMMON
#define FFETARGET_nameBLANK_COMMON "_BLNK__"
#endif

#ifndef FFETARGET_integerSMALLEST_POSITIVE
#define FFETARGET_integerSMALLEST_POSITIVE 0
#endif
#ifndef FFETARGET_integerLARGEST_POSITIVE
#define FFETARGET_integerLARGEST_POSITIVE 2147483647
#endif
#ifndef FFETARGET_integerBIG_MAGICAL
#define FFETARGET_integerBIG_MAGICAL 020000000000	/* 2147483648 */
#endif
#ifndef FFETARGET_integerALMOST_BIG_MAGICAL
#define FFETARGET_integerALMOST_BIG_MAGICAL 214748364
#endif
#ifndef FFETARGET_integerALMOST_BIG_OVERFLOW_BINARY
#define FFETARGET_integerALMOST_BIG_OVERFLOW_BINARY 0x80000000
#endif
#ifndef FFETARGET_integerALMOST_BIG_OVERFLOW_HEX
#define FFETARGET_integerALMOST_BIG_OVERFLOW_HEX 0x10000000
#endif
#ifndef FFETARGET_integerALMOST_BIG_OVERFLOW_OCTAL
#define FFETARGET_integerALMOST_BIG_OVERFLOW_OCTAL 0x20000000
#endif
#ifndef FFETARGET_integerFINISH_BIG_MAGICAL
#define FFETARGET_integerFINISH_BIG_MAGICAL 8
#endif
#ifndef FFETARGET_integerFINISH_BIG_OVERFLOW_BINARY
#define FFETARGET_integerFINISH_BIG_OVERFLOW_BINARY 0
#endif
#ifndef FFETARGET_integerFINISH_BIG_OVERFLOW_HEX
#define FFETARGET_integerFINISH_BIG_OVERFLOW_HEX 0
#endif
#ifndef FFETARGET_integerFINISH_BIG_OVERFLOW_OCTAL
#define FFETARGET_integerFINISH_BIG_OVERFLOW_OCTAL 0
#endif

#ifndef FFETARGET_offsetNONE
#define FFETARGET_offsetNONE 0	/* Not used by FFE, for backend if needed. */
#endif

#define FFETARGET_okINTEGER1 1
#define FFETARGET_okINTEGER2 1
#define FFETARGET_okINTEGER3 1
#define FFETARGET_okINTEGER4 1
#define FFETARGET_okLOGICAL1 1
#define FFETARGET_okLOGICAL2 1
#define FFETARGET_okLOGICAL3 1
#define FFETARGET_okLOGICAL4 1
#define FFETARGET_okREAL1 1
#define FFETARGET_okREAL2 1
#define FFETARGET_okREAL3 0
#define FFETARGET_okREALQUAD FFETARGET_okREAL3
#define FFETARGET_okCOMPLEX1 1
#define FFETARGET_okCOMPLEX2 1
#define FFETARGET_okCOMPLEX3 0
#define FFETARGET_okCOMPLEXDOUBLE FFETARGET_okCOMPLEX2
#define FFETARGET_okCOMPLEXQUAD FFETARGET_okCOMPLEX3
#define FFETARGET_okCHARACTER1 1

#define FFETARGET_f2cTYUNKNOWN 0
#define FFETARGET_f2cTYADDR 1
#define FFETARGET_f2cTYSHORT 2
#define FFETARGET_f2cTYLONG 3
#define FFETARGET_f2cTYREAL 4
#define FFETARGET_f2cTYDREAL 5
#define FFETARGET_f2cTYCOMPLEX 6
#define FFETARGET_f2cTYDCOMPLEX 7
#define FFETARGET_f2cTYLOGICAL 8
#define FFETARGET_f2cTYCHAR 9
#define FFETARGET_f2cTYSUBR 10
#define FFETARGET_f2cTYINT1 11
#define FFETARGET_f2cTYLOGICAL1 12
#define FFETARGET_f2cTYLOGICAL2 13
#define FFETARGET_f2cTYQUAD 14

#if !defined(__alpha__) && (!defined (_ARCH_PPC) || !defined (__64BIT__)) && (!defined(__sparc__) || (!defined(__sparcv9) && !defined(__arch64__))) && (!defined(__ia64__) || !defined(__LP64__))
#define FFETARGET_32bit_longs
#endif

/* Typedefs. */

typedef unsigned char ffetargetAlign;	/* ffetargetOffset for alignment. */
#define ffetargetAlign_f ""
typedef long ffetargetCharacterSize;
#define ffetargetCharacterSize_f "l"
typedef void (*ffetargetCopyfunc) (void *, void *, size_t);
typedef ffetargetCharacterSize ffetargetHollerithSize;
#define ffetargetHollerithSize_f "l"
typedef long long ffetargetOffset;
#define ffetargetOffset_f "ll"

#if FFETARGET_okINTEGER1
#ifdef FFETARGET_32bit_longs
typedef long int ffetargetInteger1;
#define ffetargetInteger1_f "l"
#else
typedef int ffetargetInteger1;
#define ffetargetInteger1_f ""
#endif
#endif
#if FFETARGET_okINTEGER2
typedef signed char ffetargetInteger2;
#define ffetargetInteger2_f ""
#endif
#if FFETARGET_okINTEGER3
typedef short int ffetargetInteger3;
#define ffetargetInteger3_f ""
#endif
#if FFETARGET_okINTEGER4
typedef long long int ffetargetInteger4;
#define ffetargetInteger4_f "ll"
#endif
#if FFETARGET_okINTEGER5
typedef ? ffetargetInteger5;
#define ffetargetInteger5_f
?
#endif
#if FFETARGET_okINTEGER6
typedef ? ffetargetInteger6;
#define ffetargetInteger6_f
?
#endif
#if FFETARGET_okINTEGER7
typedef ? ffetargetInteger7;
#define ffetargetInteger7_f
?
#endif
#if FFETARGET_okINTEGER8
typedef ? ffetargetInteger8;
#define ffetargetInteger8_f
?
#endif
#if FFETARGET_okLOGICAL1
#ifdef FFETARGET_32bit_longs
typedef long int ffetargetLogical1;
#define ffetargetLogical1_f "l"
#else
typedef int ffetargetLogical1;
#define ffetargetLogical1_f ""
#endif
#endif
#if FFETARGET_okLOGICAL2
typedef signed char ffetargetLogical2;
#define ffetargetLogical2_f ""
#endif
#if FFETARGET_okLOGICAL3
typedef short int ffetargetLogical3;
#define ffetargetLogical3_f ""
#endif
#if FFETARGET_okLOGICAL4
typedef long long int ffetargetLogical4;
#define ffetargetLogical4_f "ll"
#endif
#if FFETARGET_okLOGICAL5
typedef ? ffetargetLogical5;
#define ffetargetLogical5_f
?
#endif
#if FFETARGET_okLOGICAL6
typedef ? ffetargetLogical6;
#define ffetargetLogical6_f
?
#endif
#if FFETARGET_okLOGICAL7
typedef ? ffetargetLogical7;
#define ffetargetLogical7_f
?
#endif
#if FFETARGET_okLOGICAL8
typedef ? ffetargetLogical8;
#define ffetargetLogical8_f
?
#endif
#if FFETARGET_okREAL1
#ifdef REAL_ARITHMETIC
#ifdef FFETARGET_32bit_longs
typedef long int ffetargetReal1;
#define ffetargetReal1_f "l"
#define ffetarget_cvt_r1_to_rv_ REAL_VALUE_UNTO_TARGET_SINGLE
#define ffetarget_cvt_rv_to_r1_ REAL_VALUE_TO_TARGET_SINGLE
#else
typedef int ffetargetReal1;
#define ffetargetReal1_f ""
#define ffetarget_cvt_r1_to_rv_(in) \
  ({ REAL_VALUE_TYPE _rv; \
     _rv = REAL_VALUE_UNTO_TARGET_SINGLE ((long) (in)); \
     _rv; })
#define ffetarget_cvt_rv_to_r1_(in, out) \
  ({ long _tmp; \
     REAL_VALUE_TO_TARGET_SINGLE ((in), _tmp); \
     (out) = (ffetargetReal1) _tmp; })
#endif
#else	/* REAL_ARITHMETIC */
typedef float ffetargetReal1;
#define ffetargetReal1_f ""
#endif	/* REAL_ARITHMETIC */
#endif
#if FFETARGET_okREAL2
#ifdef REAL_ARITHMETIC
#ifdef FFETARGET_32bit_longs
typedef struct
  {
    long int v[2];
  }
ffetargetReal2;
#define ffetargetReal2_f "l"
#define ffetarget_cvt_r2_to_rv_ REAL_VALUE_UNTO_TARGET_DOUBLE
#define ffetarget_cvt_rv_to_r2_ REAL_VALUE_TO_TARGET_DOUBLE
#else
typedef struct
  {
    int v[2];
  }
ffetargetReal2;
#define ffetargetReal2_f ""
#define ffetarget_cvt_r2_to_rv_(in) \
  ({ REAL_VALUE_TYPE _rv; \
     long _tmp[2]; \
     _tmp[0] = (in)[0]; \
     _tmp[1] = (in)[1]; \
     _rv = REAL_VALUE_UNTO_TARGET_DOUBLE (_tmp); \
     _rv; })
#define ffetarget_cvt_rv_to_r2_(in, out) \
  ({ long _tmp[2]; \
     REAL_VALUE_TO_TARGET_DOUBLE ((in), _tmp); \
     (out)[0] = (int) (_tmp[0]); \
     (out)[1] = (int) (_tmp[1]); })
#endif
#else
typedef double ffetargetReal2;
#define ffetargetReal2_f ""
#endif
#endif
#if FFETARGET_okREAL3
#ifdef REAL_ARITHMETIC
typedef long ffetargetReal3[?];
#else
typedef ? ffetargetReal3;
#define ffetargetReal3_f
#endif
?
#endif
#if FFETARGET_okREAL4
#ifdef REAL_ARITHMETIC
typedef long ffetargetReal4[?];
#else
typedef ? ffetargetReal4;
#define ffetargetReal4_f
#endif
?
#endif
#if FFETARGET_okREAL5
#ifdef REAL_ARITHMETIC
typedef long ffetargetReal5[?];
#else
typedef ? ffetargetReal5;
#define ffetargetReal5_f
#endif
?
#endif
#if FFETARGET_okREAL6
#ifdef REAL_ARITHMETIC
typedef long ffetargetReal6[?];
#else
typedef ? ffetargetReal6;
#define ffetargetReal6_f
#endif
?
#endif
#if FFETARGET_okREAL7
#ifdef REAL_ARITHMETIC
typedef long ffetargetReal7[?];
#else
typedef ? ffetargetReal7;
#define ffetargetReal7_f
#endif
?
#endif
#if FFETARGET_okREAL8
#ifdef REAL_ARITHMETIC
typedef long ffetargetReal8[?];
#else
typedef ? ffetargetReal8;
#define ffetargetReal8_f
#endif
?
#endif
#if FFETARGET_okCOMPLEX1
struct _ffetarget_complex_1_
  {
    ffetargetReal1 real;
    ffetargetReal1 imaginary;
  };
typedef struct _ffetarget_complex_1_ ffetargetComplex1;
#endif
#if FFETARGET_okCOMPLEX2
struct _ffetarget_complex_2_
  {
    ffetargetReal2 real;
    ffetargetReal2 imaginary;
  };
typedef struct _ffetarget_complex_2_ ffetargetComplex2;
#endif
#if FFETARGET_okCOMPLEX3
struct _ffetarget_complex_3_
  {
    ffetargetReal3 real;
    ffetargetReal3 imaginary;
  };
typedef struct _ffetarget_complex_3_ ffetargetComplex3;
#endif
#if FFETARGET_okCOMPLEX4
struct _ffetarget_complex_4_
  {
    ffetargetReal4 real;
    ffetargetReal4 imaginary;
  };
typedef struct _ffetarget_complex_4_ ffetargetComplex4;
#endif
#if FFETARGET_okCOMPLEX5
struct _ffetarget_complex_5_
  {
    ffetargetReal5 real;
    ffetargetReal5 imaginary;
  };
typedef struct _ffetarget_complex_5_ ffetargetComplex5;
#endif
#if FFETARGET_okCOMPLEX6
struct _ffetarget_complex_6_
  {
    ffetargetReal6 real;
    ffetargetReal6 imaginary;
  };
typedef struct _ffetarget_complex_6_ ffetargetComplex6;
#endif
#if FFETARGET_okCOMPLEX7
struct _ffetarget_complex_7_
  {
    ffetargetReal7 real;
    ffetargetReal7 imaginary;
  };
typedef struct _ffetarget_complex_7_ ffetargetComplex7;
#endif
#if FFETARGET_okCOMPLEX8
struct _ffetarget_complex_8_
  {
    ffetargetReal8 real;
    ffetargetReal8 imaginary;
  };
typedef struct _ffetarget_complex_8_ ffetargetComplex8;
#endif
#if FFETARGET_okCHARACTER1
struct _ffetarget_char_1_
  {
    ffetargetCharacterSize length;
    unsigned char *text;
  };
typedef struct _ffetarget_char_1_ ffetargetCharacter1;
typedef unsigned char ffetargetCharacterUnit1;
#endif
#if FFETARGET_okCHARACTER2
typedef ? ffetargetCharacter2;
typedef ? ffetargetCharacterUnit2;
#endif
#if FFETARGET_okCHARACTER3
typedef ? ffetargetCharacter3;
typedef ? ffetargetCharacterUnit3;
#endif
#if FFETARGET_okCHARACTER4
typedef ? ffetargetCharacter4;
typedef ? ffetargetCharacterUnit4;
#endif
#if FFETARGET_okCHARACTER5
typedef ? ffetargetCharacter5;
typedef ? ffetargetCharacterUnit5;
#endif
#if FFETARGET_okCHARACTER6
typedef ? ffetargetCharacter6;
typedef ? ffetargetCharacterUnit6;
#endif
#if FFETARGET_okCHARACTER7
typedef ? ffetargetCharacter7;
typedef ? ffetargetCharacterUnit7;
#endif
#if FFETARGET_okCHARACTER8
typedef ? ffetargetCharacter8;
typedef ? ffetargetCharacterUnit8;
#endif

typedef unsigned long long int ffetargetTypeless;

struct _ffetarget_hollerith_
  {
    ffetargetHollerithSize length;
    unsigned char *text;
  };
typedef struct _ffetarget_hollerith_ ffetargetHollerith;

typedef ffetargetCharacter1 ffetargetCharacterDefault;
typedef ffetargetComplex1 ffetargetComplexDefault;
#if FFETARGET_okCOMPLEXDOUBLE
typedef ffetargetComplex2 ffetargetComplexDouble;
#endif
#if FFETARGET_okCOMPLEXQUAD
typedef ffetargetComplex3 ffetargetComplexQuad;
#endif
typedef ffetargetInteger1 ffetargetIntegerDefault;
#define ffetargetIntegerDefault_f ffetargetInteger1_f
typedef ffetargetLogical1 ffetargetLogicalDefault;
#define ffetargetLogicalDefault_f ffetargetLogical1_f
typedef ffetargetReal1 ffetargetRealDefault;
#define ffetargetRealDefault_f ffetargetReal1_f
typedef ffetargetReal2 ffetargetRealDouble;
#define ffetargetRealDouble_f ffetargetReal2_f
#if FFETARGET_okREALQUAD
typedef ffetargetReal3 ffetargetRealQuad;
#define ffetargetRealQuad_f ffetargetReal3_f
#endif

/* Include files needed by this one. */

#include "bad.h"
#include "info.h"
#include "lex.h"
#include "malloc.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */

extern char ffetarget_string_[40];	/* Temp for ascii-to-double (atof). */
extern HOST_WIDE_INT ffetarget_long_val_;
extern HOST_WIDE_INT ffetarget_long_junk_;

/* Declare functions with prototypes. */

void ffetarget_aggregate_info (ffeinfoBasictype *ebt, ffeinfoKindtype *ekt,
			       ffetargetAlign *units, ffeinfoBasictype abt,
			       ffeinfoKindtype akt);
ffetargetAlign ffetarget_align (ffetargetAlign *updated_alignment,
				ffetargetAlign *updated_modulo,
				ffetargetOffset offset,
				ffetargetAlign alignment,
				ffetargetAlign modulo);
#if FFETARGET_okCHARACTER1
bool ffetarget_character1 (ffetargetCharacter1 *val, ffelexToken character,
			   mallocPool pool);
int ffetarget_cmp_character1 (ffetargetCharacter1 l, ffetargetCharacter1 r);
ffebad ffetarget_concatenate_character1 (ffetargetCharacter1 *res,
					 ffetargetCharacter1 l,
					 ffetargetCharacter1 r,
					 mallocPool pool,
					 ffetargetCharacterSize *len);
ffebad ffetarget_convert_character1_character1 (ffetargetCharacter1 *res,
					    ffetargetCharacterSize res_size,
						ffetargetCharacter1 l,
						mallocPool pool);
ffebad ffetarget_convert_character1_hollerith (ffetargetCharacter1 *res,
					    ffetargetCharacterSize res_size,
					       ffetargetHollerith l,
					       mallocPool pool);
ffebad ffetarget_convert_character1_integer4 (ffetargetCharacter1 *res,
					      ffetargetCharacterSize res_size,
					      ffetargetInteger4 l,
					      mallocPool pool);
ffebad ffetarget_convert_character1_logical4 (ffetargetCharacter1 *res,
					      ffetargetCharacterSize res_size,
					      ffetargetLogical4 l,
					      mallocPool pool);
ffebad ffetarget_convert_character1_typeless (ffetargetCharacter1 *res,
					    ffetargetCharacterSize res_size,
					      ffetargetTypeless l,
					      mallocPool pool);
ffebad ffetarget_eq_character1 (bool *res, ffetargetCharacter1 l,
				ffetargetCharacter1 r);
ffebad ffetarget_le_character1 (bool *res, ffetargetCharacter1 l,
				ffetargetCharacter1 r);
ffebad ffetarget_ge_character1 (bool *res, ffetargetCharacter1 l,
				ffetargetCharacter1 r);
ffebad ffetarget_gt_character1 (bool *res, ffetargetCharacter1 l,
				ffetargetCharacter1 r);
ffebad ffetarget_lt_character1 (bool *res, ffetargetCharacter1 l,
				ffetargetCharacter1 r);
ffebad ffetarget_ne_character1 (bool *res, ffetargetCharacter1 l,
				ffetargetCharacter1 r);
ffebad ffetarget_substr_character1 (ffetargetCharacter1 *res,
				    ffetargetCharacter1 l,
				    ffetargetCharacterSize first,
				    ffetargetCharacterSize last,
				    mallocPool pool,
				    ffetargetCharacterSize *len);
#endif
int ffetarget_cmp_hollerith (ffetargetHollerith l, ffetargetHollerith r);
bool ffetarget_hollerith (ffetargetHollerith *val, ffelexToken hollerith,
			  mallocPool pool);
int ffetarget_cmp_typeless (ffetargetTypeless l, ffetargetTypeless r);
ffebad ffetarget_convert_any_character1_ (char *res, size_t size,
					  ffetargetCharacter1 l);
ffebad ffetarget_convert_any_hollerith_ (char *res, size_t size,
					 ffetargetHollerith l);
ffebad ffetarget_convert_any_typeless_ (char *res, size_t size,
					ffetargetTypeless l);
#if FFETARGET_okCOMPLEX1
ffebad ffetarget_divide_complex1 (ffetargetComplex1 *res, ffetargetComplex1 l,
				  ffetargetComplex1 r);
#endif
#if FFETARGET_okCOMPLEX2
ffebad ffetarget_divide_complex2 (ffetargetComplex2 *res, ffetargetComplex2 l,
				  ffetargetComplex2 r);
#endif
#if FFETARGET_okCOMPLEX3
ffebad ffetarget_divide_complex3 (ffetargetComplex3 *res, ffetargetComplex3 l,
				  ffetargetComplex3 r);
#endif
#if FFETARGET_okCOMPLEX4
ffebad ffetarget_divide_complex4 (ffetargetComplex4 *res, ffetargetComplex4 l,
				  ffetargetComplex4 r);
#endif
#if FFETARGET_okCOMPLEX5
ffebad ffetarget_divide_complex5 (ffetargetComplex5 *res, ffetargetComplex5 l,
				  ffetargetComplex5 r);
#endif
#if FFETARGET_okCOMPLEX6
ffebad ffetarget_divide_complex6 (ffetargetComplex6 *res, ffetargetComplex6 l,
				  ffetargetComplex6 r);
#endif
#if FFETARGET_okCOMPLEX7
ffebad ffetarget_divide_complex7 (ffetargetComplex7 *res, ffetargetComplex7 l,
				  ffetargetComplex7 r);
#endif
#if FFETARGET_okCOMPLEX8
ffebad ffetarget_divide_complex8 (ffetargetComplex8 *res, ffetargetComplex8 l,
				  ffetargetComplex8 r);
#endif
#if FFETARGET_okINTEGER1
bool ffetarget_integer1 (ffetargetInteger1 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER2
bool ffetarget_integer2 (ffetargetInteger2 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER3
bool ffetarget_integer3 (ffetargetInteger3 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER4
bool ffetarget_integer4 (ffetargetInteger4 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER5
bool ffetarget_integer5 (ffetargetInteger5 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER6
bool ffetarget_integer6 (ffetargetInteger6 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER7
bool ffetarget_integer7 (ffetargetInteger7 *val, ffelexToken integer);
#endif
#if FFETARGET_okINTEGER8
bool ffetarget_integer8 (ffetargetInteger8 *val, ffelexToken integer);
#endif
bool ffetarget_integerbinary (ffetargetIntegerDefault *val,
			     ffelexToken integer);
bool ffetarget_integerhex (ffetargetIntegerDefault *val,
			     ffelexToken integer);
bool ffetarget_integeroctal (ffetargetIntegerDefault *val,
			     ffelexToken integer);
void ffetarget_integer_bad_magical (ffelexToken t);
void ffetarget_integer_bad_magical_binary (ffelexToken integer, ffelexToken minus);
void ffetarget_integer_bad_magical_precedence (ffelexToken integer,
					       ffelexToken uminus,
					       ffelexToken higher_op);
void ffetarget_integer_bad_magical_precedence_binary (ffelexToken integer,
						      ffelexToken minus,
						      ffelexToken higher_op);
#if FFETARGET_okCHARACTER1
bool ffetarget_iszero_character1 (ffetargetCharacter1 constant);
#endif
bool ffetarget_iszero_hollerith (ffetargetHollerith constant);
void ffetarget_layout (const char *error_text, ffetargetAlign *alignment,
		       ffetargetAlign *modulo, ffetargetOffset *size,
		       ffeinfoBasictype bt, ffeinfoKindtype kt,
		       ffetargetCharacterSize charsize,
		       ffetargetIntegerDefault num_elements);
#if FFETARGET_okCOMPLEX1
ffebad ffetarget_multiply_complex1 (ffetargetComplex1 *res,
				    ffetargetComplex1 l,
				    ffetargetComplex1 r);
#endif
#if FFETARGET_okCOMPLEX2
ffebad ffetarget_multiply_complex2 (ffetargetComplex2 *res,
				    ffetargetComplex2 l,
				    ffetargetComplex2 r);
#endif
#if FFETARGET_okCOMPLEX3
ffebad ffetarget_multiply_complex3 (ffetargetComplex3 *res,
				    ffetargetComplex3 l,
				    ffetargetComplex3 r);
#endif
#if FFETARGET_okCOMPLEX4
ffebad ffetarget_multiply_complex4 (ffetargetComplex4 *res,
				    ffetargetComplex4 l,
				    ffetargetComplex4 r);
#endif
#if FFETARGET_okCOMPLEX5
ffebad ffetarget_multiply_complex5 (ffetargetComplex5 *res,
				    ffetargetComplex5 l,
				    ffetargetComplex5 r);
#endif
#if FFETARGET_okCOMPLEX6
ffebad ffetarget_multiply_complex6 (ffetargetComplex6 *res,
				    ffetargetComplex6 l,
				    ffetargetComplex6 r);
#endif
#if FFETARGET_okCOMPLEX7
ffebad ffetarget_multiply_complex7 (ffetargetComplex7 *res,
				    ffetargetComplex7 l,
				    ffetargetComplex7 r);
#endif
#if FFETARGET_okCOMPLEX8
ffebad ffetarget_multiply_complex8 (ffetargetComplex8 *res,
				    ffetargetComplex8 l,
				    ffetargetComplex8 r);
#endif
ffebad ffetarget_power_complexdefault_integerdefault (ffetargetComplexDefault *res,
						  ffetargetComplexDefault l,
						 ffetargetIntegerDefault r);
#if FFETARGET_okCOMPLEXDOUBLE
ffebad ffetarget_power_complexdouble_integerdefault (ffetargetComplexDouble *res,
						   ffetargetComplexDouble l,
						 ffetargetIntegerDefault r);
#endif
ffebad ffetarget_power_integerdefault_integerdefault (ffetargetIntegerDefault *res,
						  ffetargetIntegerDefault l,
						 ffetargetIntegerDefault r);
ffebad ffetarget_power_realdefault_integerdefault (ffetargetRealDefault *res,
						   ffetargetRealDefault l,
						 ffetargetIntegerDefault r);
ffebad ffetarget_power_realdouble_integerdefault (ffetargetRealDouble *res,
						  ffetargetRealDouble l,
						  ffetargetIntegerDefault r);
void ffetarget_print_binary (FILE *f, ffetargetTypeless val);
void ffetarget_print_character1 (FILE *f, ffetargetCharacter1 val);
void ffetarget_print_hollerith (FILE *f, ffetargetHollerith val);
void ffetarget_print_octal (FILE *f, ffetargetTypeless val);
void ffetarget_print_hex (FILE *f, ffetargetTypeless val);
#if FFETARGET_okREAL1
bool ffetarget_real1 (ffetargetReal1 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL2
bool ffetarget_real2 (ffetargetReal2 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL3
bool ffetarget_real3 (ffetargetReal3 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL4
bool ffetarget_real4 (ffetargetReal4 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL5
bool ffetarget_real5 (ffetargetReal5 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL6
bool ffetarget_real6 (ffetargetReal6 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL7
bool ffetarget_real7 (ffetargetReal7 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
#if FFETARGET_okREAL8
bool ffetarget_real8 (ffetargetReal8 *value, ffelexToken integer,
		      ffelexToken decimal, ffelexToken fraction,
		      ffelexToken exponent, ffelexToken exponent_sign,
		      ffelexToken exponent_digits);
#endif
bool ffetarget_typeless_binary (ffetargetTypeless *value, ffelexToken token);
bool ffetarget_typeless_octal (ffetargetTypeless *value, ffelexToken token);
bool ffetarget_typeless_hex (ffetargetTypeless *value, ffelexToken token);
void ffetarget_verify_character1 (mallocPool pool, ffetargetCharacter1 val);
int ffetarget_num_digits_ (ffelexToken t);
void *ffetarget_memcpy_ (void *dst, void *src, size_t len);

/* Define macros. */

#if BUILT_FOR_280
#define FFETARGET_REAL_VALUE_FROM_INT_(resr, lf, kt) \
  REAL_VALUE_FROM_INT (resr, (long) lf, (long) ((lf < 0) ? -1 : 0), ((kt == 1) ? SFmode : DFmode))
#else
#define FFETARGET_REAL_VALUE_FROM_INT_(resr, lf, kt) \
  REAL_VALUE_FROM_INT (resr, (long) lf, (long) ((lf < 0) ? -1 : 0))
#endif

#ifdef REAL_ARITHMETIC
#define ffetarget_add_complex1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri, resr, resi; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     li = ffetarget_cvt_r1_to_rv_ ((l).imaginary); \
     rr = ffetarget_cvt_r1_to_rv_ ((r).real); \
     ri = ffetarget_cvt_r1_to_rv_ ((r).imaginary); \
     REAL_ARITHMETIC (resr, PLUS_EXPR, lr, rr); \
     REAL_ARITHMETIC (resi, PLUS_EXPR, li, ri); \
     ffetarget_cvt_rv_to_r1_ (resr, (res)->real); \
     ffetarget_cvt_rv_to_r1_ (resi, (res)->imaginary); \
     FFEBAD; })
#define ffetarget_add_complex2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri, resr, resi; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     li = ffetarget_cvt_r2_to_rv_ (&((l).imaginary.v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).real.v[0])); \
     ri = ffetarget_cvt_r2_to_rv_ (&((r).imaginary.v[0])); \
     REAL_ARITHMETIC (resr, PLUS_EXPR, lr, rr); \
     REAL_ARITHMETIC (resi, PLUS_EXPR, li, ri); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->real.v[0])); \
     ffetarget_cvt_rv_to_r2_ (resi, &((res)->imaginary.v[0])); \
     FFEBAD; })
#else
#define ffetarget_add_complex1(res,l,r) \
  ((res)->real = (l).real + (r).real, \
   (res)->imaginary = (l).imaginary + (r).imaginary, FFEBAD)
#define ffetarget_add_complex2(res,l,r) \
  ((res)->real = (l).real + (r).real, \
   (res)->imaginary = (l).imaginary + (r).imaginary, FFEBAD)
#endif
#define ffetarget_add_integer1(res,l,r) (*(res) = (l) + (r), FFEBAD)
#define ffetarget_add_integer2(res,l,r) (*(res) = (l) + (r), FFEBAD)
#define ffetarget_add_integer3(res,l,r) (*(res) = (l) + (r), FFEBAD)
#define ffetarget_add_integer4(res,l,r) (*(res) = (l) + (r), FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_add_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     REAL_ARITHMETIC (resr, PLUS_EXPR, lr, rr); \
     ffetarget_cvt_rv_to_r1_ (resr, *(res)); \
     FFEBAD; })
#define ffetarget_add_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     REAL_ARITHMETIC (resr, PLUS_EXPR, lr, rr); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->v[0])); \
     FFEBAD; })
#else
#define ffetarget_add_real1(res,l,r) (*(res) = (l) + (r), FFEBAD)
#define ffetarget_add_real2(res,l,r) (*(res) = (l) + (r), FFEBAD)
#endif
#define ffetarget_aggregate_ptr_memcpy(dbt,dkt,sbt,skt) \
  ((ffetargetCopyfunc) ffetarget_memcpy_)
#define ffetarget_and_integer1(res,l,r) (*(res) = (l) & (r), FFEBAD)
#define ffetarget_and_integer2(res,l,r) (*(res) = (l) & (r), FFEBAD)
#define ffetarget_and_integer3(res,l,r) (*(res) = (l) & (r), FFEBAD)
#define ffetarget_and_integer4(res,l,r) (*(res) = (l) & (r), FFEBAD)
#define ffetarget_and_logical1(res,l,r) (*(res) = (l) && (r), FFEBAD)
#define ffetarget_and_logical2(res,l,r) (*(res) = (l) && (r), FFEBAD)
#define ffetarget_and_logical3(res,l,r) (*(res) = (l) && (r), FFEBAD)
#define ffetarget_and_logical4(res,l,r) (*(res) = (l) && (r), FFEBAD)
#define ffetarget_binarymil(v,t) ffetarget_typeless_binary (v, t)
#define ffetarget_binaryvxt(v,t) ffetarget_typeless_binary (v, t)
#define ffetarget_cmp_integer1(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_integer2(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_integer3(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_integer4(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_logical1(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_logical2(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_logical3(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_logical4(l,r) ((l) == (r) ? 0 : ((l) < (r) ? -1 : 1))
#define ffetarget_cmp_real1(l,r) memcmp (&(l), &(r), sizeof(l))
#define ffetarget_cmp_real2(l,r) memcmp (&(l), &(r), sizeof(l))
#define ffetarget_cmp_real3(l,r) memcmp (&(l), &(r), sizeof(l))
#define ffetarget_cmp_typeless(l,r) \
  memcmp (&(l), &(r), sizeof ((l)))
#define ffetarget_convert_character1_integer1(res,res_size,l,pool) \
        ffetarget_convert_character1_integer4(res,res_size,(ffetargetInteger4)l,pool)
#define ffetarget_convert_character1_integer2(res,res_size,l,pool) \
        ffetarget_convert_character1_integer4(res,res_size,(ffetargetInteger4)l,pool)
#define ffetarget_convert_character1_integer3(res,res_size,l,pool) \
        ffetarget_convert_character1_integer4(res,res_size,(ffetargetInteger4)l,pool)
#define ffetarget_convert_character1_logical1(res,res_size,l,pool) \
        ffetarget_convert_character1_logical4(res,res_size,(ffetargetLogical4)l,pool)
#define ffetarget_convert_character1_logical2(res,res_size,l,pool) \
        ffetarget_convert_character1_logical4(res,res_size,(ffetargetLogical4)l,pool)
#define ffetarget_convert_character1_logical3(res,res_size,l,pool) \
        ffetarget_convert_character1_logical4(res,res_size,(ffetargetLogical4)l,pool)
#define ffetarget_convert_complex1_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_complex1_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_complex1_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex1_complex2(res,l) \
  ({ REAL_VALUE_TYPE lr, li; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     li = ffetarget_cvt_r2_to_rv_ (&((l).imaginary.v[0])); \
     ffetarget_cvt_rv_to_r1_ (lr, (res)->real); \
     ffetarget_cvt_rv_to_r1_ (li, (res)->imaginary), \
     FFEBAD; })
#else
#define ffetarget_convert_complex1_complex2(res,l) \
  ((res)->real = (l).real, (res)->imaginary = (l).imaginary, FFEBAD)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex1_integer(res,l) \
  ({ REAL_VALUE_TYPE resi, resr; \
     ffetargetInteger1 lf = (l); \
     FFETARGET_REAL_VALUE_FROM_INT_ (resr, lf, 1); \
     resi = dconst0; \
     ffetarget_cvt_rv_to_r1_ (resr, (res)->real); \
     ffetarget_cvt_rv_to_r1_ (resi, (res)->imaginary); \
     FFEBAD; })
#else
#define ffetarget_convert_complex1_integer(res,l) \
  ((res)->real = (l), (res)->imaginary = 0, FFEBAD)
#endif
#define ffetarget_convert_complex1_integer1 ffetarget_convert_complex1_integer
#define ffetarget_convert_complex1_integer2 ffetarget_convert_complex1_integer
#define ffetarget_convert_complex1_integer3 ffetarget_convert_complex1_integer
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex1_integer4(res,l) FFEBAD_NOCANDO
#else
#define ffetarget_convert_complex1_integer4 ffetarget_convert_complex1_integer
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex1_real1(res,l) \
  ((res)->real = (l), \
   ffetarget_cvt_rv_to_r1_ (dconst0, (res)->imaginary), \
   FFEBAD)
#define ffetarget_convert_complex1_real2(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     ffetarget_cvt_rv_to_r1_ (lr, (res)->real); \
     ffetarget_cvt_rv_to_r1_ (dconst0, (res)->imaginary), \
     FFEBAD; })
#else
#define ffetarget_convert_complex1_real1(res,l) \
  ((res)->real = (l), (res)->imaginary = 0, FFEBAD)
#define ffetarget_convert_complex1_real2(res,l) \
  ((res)->real = (l), (res)->imaginary = 0, FFEBAD)
#endif
#define ffetarget_convert_complex2_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_complex2_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_complex2_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex2_complex1(res,l) \
  ({ REAL_VALUE_TYPE lr, li; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     li = ffetarget_cvt_r1_to_rv_ ((l).imaginary); \
     ffetarget_cvt_rv_to_r2_ (lr, &((res)->real.v[0])); \
     ffetarget_cvt_rv_to_r2_ (li, &((res)->imaginary.v[0])), \
     FFEBAD; })
#else
#define ffetarget_convert_complex2_complex1(res,l) \
  ((res)->real = (l).real, (res)->imaginary = (l).imaginary, FFEBAD)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex2_integer(res,l) \
  ({ REAL_VALUE_TYPE resi, resr; \
     ffetargetInteger1 lf = (l); \
     FFETARGET_REAL_VALUE_FROM_INT_ (resr, lf, 2); \
     resi = dconst0; \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->real.v[0])); \
     ffetarget_cvt_rv_to_r2_ (resi, &((res)->imaginary.v[0])); \
     FFEBAD; })
#else
#define ffetarget_convert_complex2_integer(res,l) \
  ((res)->real = (l), (res)->imaginary = 0, FFEBAD)
#endif
#define ffetarget_convert_complex2_integer1 ffetarget_convert_complex2_integer
#define ffetarget_convert_complex2_integer2 ffetarget_convert_complex2_integer
#define ffetarget_convert_complex2_integer3 ffetarget_convert_complex2_integer
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex2_integer4(res,l) FFEBAD_NOCANDO
#else
#define ffetarget_convert_complex2_integer4 ffetarget_convert_complex2_integer
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_complex2_real1(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r1_to_rv_ (l); \
     ffetarget_cvt_rv_to_r2_ (lr, &((res)->real.v[0])); \
     ffetarget_cvt_rv_to_r2_ (dconst0, &((res)->imaginary.v[0])), \
     FFEBAD; })
#define ffetarget_convert_complex2_real2(res,l) \
  ((res)->real = (l), \
   ffetarget_cvt_rv_to_r2_ (dconst0, &((res)->imaginary.v[0])), \
   FFEBAD)
#else
#define ffetarget_convert_complex2_real1(res,l) \
  ((res)->real = (l), (res)->imaginary = 0, FFEBAD)
#define ffetarget_convert_complex2_real2(res,l) \
  ((res)->real = (l), (res)->imaginary = 0, FFEBAD)
#endif
#define ffetarget_convert_integer2_character1(res,l) \
        ffetarget_convert_integer1_character1(res,l)
#define ffetarget_convert_integer2_complex1(res,l) \
        ffetarget_convert_integer1_complex1(res,l)
#define ffetarget_convert_integer2_complex2(res,l) \
        ffetarget_convert_integer1_complex2(res,l)
#define ffetarget_convert_integer2_hollerith(res,l) \
        ffetarget_convert_integer1_hollerith(res,l)
#define ffetarget_convert_integer2_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer2_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer2_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer2_logical1(res,l) \
        ffetarget_convert_integer1_logical1(res,l)
#define ffetarget_convert_integer2_logical2(res,l) \
        ffetarget_convert_integer2_logical1(res,l)
#define ffetarget_convert_integer2_logical3(res,l) \
        ffetarget_convert_integer2_logical1(res,l)
#define ffetarget_convert_integer2_logical4(res,l) \
        ffetarget_convert_integer2_logical1(res,l)
#define ffetarget_convert_integer2_real1(res,l) \
        ffetarget_convert_integer1_real1(res,l)
#define ffetarget_convert_integer2_real2(res,l) \
        ffetarget_convert_integer1_real2(res,l)
#define ffetarget_convert_integer2_typeless(res,l) \
        ffetarget_convert_integer1_typeless(res,l)
#define ffetarget_convert_integer3_character1(res,l) \
        ffetarget_convert_integer1_character1(res,l)
#define ffetarget_convert_integer3_complex1(res,l) \
        ffetarget_convert_integer1_complex1(res,l)
#define ffetarget_convert_integer3_complex2(res,l) \
        ffetarget_convert_integer1_complex2(res,l)
#define ffetarget_convert_integer3_hollerith(res,l) \
        ffetarget_convert_integer1_hollerith(res,l)
#define ffetarget_convert_integer3_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer3_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer3_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer3_logical1(res,l) \
        ffetarget_convert_integer1_logical1(res,l)
#define ffetarget_convert_integer3_logical2(res,l) \
        ffetarget_convert_integer3_logical1(res,l)
#define ffetarget_convert_integer3_logical3(res,l) \
        ffetarget_convert_integer3_logical1(res,l)
#define ffetarget_convert_integer3_logical4(res,l) \
        ffetarget_convert_integer3_logical1(res,l)
#define ffetarget_convert_integer3_real1(res,l) \
        ffetarget_convert_integer1_real1(res,l)
#define ffetarget_convert_integer3_real2(res,l) \
        ffetarget_convert_integer1_real2(res,l)
#define ffetarget_convert_integer3_typeless(res,l) \
        ffetarget_convert_integer1_typeless(res,l)
#define ffetarget_convert_integer4_character1(res,l) \
        ffetarget_convert_integer1_character1(res,l)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_integer4_complex1(res,l) FFEBAD_NOCANDO
#define ffetarget_convert_integer4_complex2(res,l) FFEBAD_NOCANDO
#else
#define ffetarget_convert_integer4_complex1(res,l) \
        ffetarget_convert_integer1_complex1(res,l)
#define ffetarget_convert_integer4_complex2(res,l) \
        ffetarget_convert_integer1_complex2(res,l)
#endif
#define ffetarget_convert_integer4_hollerith(res,l) \
        ffetarget_convert_integer1_hollerith(res,l)
#define ffetarget_convert_integer4_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer4_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer4_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer4_logical1(res,l) \
        ffetarget_convert_integer1_logical1(res,l)
#define ffetarget_convert_integer4_logical2(res,l) \
        ffetarget_convert_integer1_logical1(res,l)
#define ffetarget_convert_integer4_logical3(res,l) \
        ffetarget_convert_integer1_logical1(res,l)
#define ffetarget_convert_integer4_logical4(res,l) \
        ffetarget_convert_integer1_logical1(res,l)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_integer4_real1(res,l) FFEBAD_NOCANDO
#define ffetarget_convert_integer4_real2(res,l) FFEBAD_NOCANDO
#else
#define ffetarget_convert_integer4_real1(res,l) \
        ffetarget_convert_integer1_real1(res,l)
#define ffetarget_convert_integer4_real2(res,l) \
        ffetarget_convert_integer1_real2(res,l)
#endif
#define ffetarget_convert_integer4_typeless(res,l) \
        ffetarget_convert_integer1_typeless(res,l)
#define ffetarget_convert_logical1_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical1_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical1_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical1_logical2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical1_logical3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical1_logical4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical1_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical1_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical1_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical1_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical2_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical2_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical2_logical1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_logical3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_logical4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical2_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical3_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical3_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical3_logical1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_logical2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_logical4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical3_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical4_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical4_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_logical4_logical1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_logical2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_logical3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_integer1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_logical4_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_integer1_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_integer1_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_integer1_integer2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_integer3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_integer4(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_logical1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_logical2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_logical3(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_logical4(res,l) (*(res) = (l), FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_integer1_real1(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r1_to_rv_ (l); \
     REAL_VALUE_TO_INT (&ffetarget_long_val_, &ffetarget_long_junk_, lr); \
     *(res) = ffetarget_long_val_; \
     FFEBAD; })
#define ffetarget_convert_integer1_real2(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     REAL_VALUE_TO_INT (&ffetarget_long_val_, &ffetarget_long_junk_, lr); \
     *(res) = ffetarget_long_val_; \
     FFEBAD; })
#define ffetarget_convert_integer1_complex1(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     REAL_VALUE_TO_INT (&ffetarget_long_val_, &ffetarget_long_junk_, lr); \
     *(res) = ffetarget_long_val_; \
     FFEBAD; })
#define ffetarget_convert_integer1_complex2(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     REAL_VALUE_TO_INT (&ffetarget_long_val_, &ffetarget_long_junk_, lr); \
     *(res) = ffetarget_long_val_; \
     FFEBAD; })
#else
#define ffetarget_convert_integer1_real1(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_real2(res,l) (*(res) = (l), FFEBAD)
#define ffetarget_convert_integer1_complex1(res,l) (*(res) = (l).real, FFEBAD)
#define ffetarget_convert_integer1_complex2(res,l) (*(res) = (l).real, FFEBAD)
#endif
#define ffetarget_convert_real1_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_real1_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_real1_integer2(res,l) \
        ffetarget_convert_real1_integer1(res,l)
#define ffetarget_convert_real1_integer3(res,l) \
        ffetarget_convert_real1_integer1(res,l)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_real1_integer4(res,l) FFEBAD_NOCANDO
#else
#define ffetarget_convert_real1_integer4(res,l) \
        ffetarget_convert_real1_integer1(res,l)
#endif
#define ffetarget_convert_real1_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_real1_complex1(res,l) (*(res) = (l).real, FFEBAD)
#define ffetarget_convert_real1_complex2(res,l) \
  ffetarget_convert_real1_real2 ((res), (l).real)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_real1_integer1(res,l) \
  ({ REAL_VALUE_TYPE resr; \
     ffetargetInteger1 lf = (l); \
     FFETARGET_REAL_VALUE_FROM_INT_ (resr, lf, 1); \
     ffetarget_cvt_rv_to_r1_ (resr, *(res)); \
     FFEBAD; })
#else
#define ffetarget_convert_real1_integer1(res,l) (*(res) = (l), FFEBAD)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_real1_real2(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     ffetarget_cvt_rv_to_r1_ (lr, *(res)); \
     FFEBAD; })
#else
#define ffetarget_convert_real1_real2(res,l) (*(res) = (l), FFEBAD)
#endif
#define ffetarget_convert_real2_character1(res,l) \
  ffetarget_convert_any_character1_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_real2_hollerith(res,l) \
  ffetarget_convert_any_hollerith_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_real2_integer2(res,l) \
        ffetarget_convert_real2_integer1(res,l)
#define ffetarget_convert_real2_integer3(res,l) \
        ffetarget_convert_real2_integer1(res,l)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_real2_integer4(res,l) FFEBAD_NOCANDO
#else
#define ffetarget_convert_real2_integer4(res,l) \
        ffetarget_convert_real2_integer1(res,l)
#endif
#define ffetarget_convert_real2_typeless(res,l) \
  ffetarget_convert_any_typeless_ ((char *) (res), sizeof(*(res)), l)
#define ffetarget_convert_real2_complex1(res,l) \
  ffetarget_convert_real2_real1 ((res), (l).real)
#define ffetarget_convert_real2_complex2(res,l) (*(res) = (l).real, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_real2_integer(res,l) \
  ({ REAL_VALUE_TYPE resr; \
     ffetargetInteger1 lf = (l); \
     FFETARGET_REAL_VALUE_FROM_INT_ (resr, lf, 2); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->v[0])); \
     FFEBAD; })
#define ffetarget_convert_real2_integer1 ffetarget_convert_real2_integer
#else
#define ffetarget_convert_real2_integer1(res,l) (*(res) = (l), FFEBAD)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_convert_real2_real1(res,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     ffetarget_cvt_rv_to_r2_ (lr, &((res)->v[0])); \
     FFEBAD; })
#else
#define ffetarget_convert_real2_real1(res,l) (*(res) = (l), FFEBAD)
#endif
#define ffetarget_divide_integer1(res,l,r) \
  (((r) == 0) ? (*(res) = 0, FFEBAD_DIV_BY_ZERO)  \
   : (*(res) = (l) / (r), FFEBAD))
#define ffetarget_divide_integer2(res,l,r) \
        ffetarget_divide_integer1(res,l,r)
#define ffetarget_divide_integer3(res,l,r) \
        ffetarget_divide_integer1(res,l,r)
#define ffetarget_divide_integer4(res,l,r) \
        ffetarget_divide_integer1(res,l,r)
#ifdef REAL_ARITHMETIC
#define ffetarget_divide_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     REAL_VALUES_EQUAL (rr, dconst0) \
       ? ({ ffetarget_cvt_rv_to_r1_ (dconst0, *(res)); \
	    FFEBAD_DIV_BY_ZERO; \
	  }) \
	 : ({ REAL_ARITHMETIC (resr, RDIV_EXPR, lr, rr); \
	      ffetarget_cvt_rv_to_r1_ (resr, *(res)); \
	      FFEBAD; \
	    }); \
	 })
#define ffetarget_divide_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     REAL_VALUES_EQUAL (rr, dconst0) \
       ? ({ ffetarget_cvt_rv_to_r2_ (dconst0, &((res)->v[0])); \
	    FFEBAD_DIV_BY_ZERO; \
	  }) \
	 : ({ REAL_ARITHMETIC (resr, RDIV_EXPR, lr, rr); \
	      ffetarget_cvt_rv_to_r2_ (resr, &((res)->v[0])); \
	      FFEBAD; \
	    }); \
	 })
#else
#define ffetarget_divide_real1(res,l,r) \
  (((r) == 0) ? (*(res) = 0, FFEBAD_DIV_BY_ZERO)  \
   : (*(res) = (l) / (r), FFEBAD))
#define ffetarget_divide_real2(res,l,r) \
  (((r) == 0) ? (*(res) = 0, FFEBAD_DIV_BY_ZERO)  \
   : (*(res) = (l) / (r), FFEBAD))
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_eq_complex1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     li = ffetarget_cvt_r1_to_rv_ ((l).imaginary); \
     rr = ffetarget_cvt_r1_to_rv_ ((r).real); \
     ri = ffetarget_cvt_r1_to_rv_ ((r).imaginary); \
     *(res) = (REAL_VALUES_EQUAL (lr, rr) && REAL_VALUES_EQUAL (li, ri)) \
       ? TRUE : FALSE; \
     FFEBAD; })
#define ffetarget_eq_complex2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     li = ffetarget_cvt_r2_to_rv_ (&((l).imaginary.v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).real.v[0])); \
     ri = ffetarget_cvt_r2_to_rv_ (&((r).imaginary.v[0])); \
     *(res) = (REAL_VALUES_EQUAL (lr, rr) && REAL_VALUES_EQUAL (li, ri)) \
       ? TRUE : FALSE; \
     FFEBAD; })
#else
#define ffetarget_eq_complex1(res,l,r) \
  (*(res) = (((l).real == (r).real) && ((l).imaginary == (r).imaginary))  \
   ? TRUE : FALSE, FFEBAD)
#define ffetarget_eq_complex2(res,l,r) \
  (*(res) = (((l).real == (r).real) && ((l).imaginary == (r).imaginary))  \
   ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_eq_integer1(res,l,r) \
  (*(res) = ((l) == (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_eq_integer2(res,l,r) \
  (*(res) = ((l) == (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_eq_integer3(res,l,r) \
  (*(res) = ((l) == (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_eq_integer4(res,l,r) \
  (*(res) = ((l) == (r)) ? TRUE : FALSE, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_eq_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     *(res) = REAL_VALUES_EQUAL (lr, rr) ? TRUE : FALSE; \
     FFEBAD; })
#define ffetarget_eq_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     *(res) = REAL_VALUES_EQUAL (lr, rr) ? TRUE : FALSE; \
     FFEBAD; })
#else
#define ffetarget_eq_real1(res,l,r) \
  (*(res) = ((l) == (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_eq_real2(res,l,r) \
  (*(res) = ((l) == (r)) ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_eqv_integer1(res,l,r) (*(res) = (l) ^ ~(r), FFEBAD)
#define ffetarget_eqv_integer2(res,l,r) (*(res) = (l) ^ ~(r), FFEBAD)
#define ffetarget_eqv_integer3(res,l,r) (*(res) = (l) ^ ~(r), FFEBAD)
#define ffetarget_eqv_integer4(res,l,r) (*(res) = (l) ^ ~(r), FFEBAD)
#define ffetarget_eqv_logical1(res,l,r) (*(res) = (l) == (r), FFEBAD)
#define ffetarget_eqv_logical2(res,l,r) (*(res) = (l) == (r), FFEBAD)
#define ffetarget_eqv_logical3(res,l,r) (*(res) = (l) == (r), FFEBAD)
#define ffetarget_eqv_logical4(res,l,r) (*(res) = (l) == (r), FFEBAD)
#define ffetarget_ge_integer1(res,l,r) \
  (*(res) = ((l) >= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ge_integer2(res,l,r) \
  (*(res) = ((l) >= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ge_integer3(res,l,r) \
  (*(res) = ((l) >= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ge_integer4(res,l,r) \
  (*(res) = ((l) >= (r)) ? TRUE : FALSE, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_ge_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     *(res) = REAL_VALUES_LESS (lr, rr) ? FALSE : TRUE; \
     FFEBAD; })
#define ffetarget_ge_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     *(res) = REAL_VALUES_LESS (lr, rr) ? FALSE : TRUE; \
     FFEBAD; })
#else
#define ffetarget_ge_real1(res,l,r) \
  (*(res) = ((l) >= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ge_real2(res,l,r) \
  (*(res) = ((l) >= (r)) ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_gt_integer1(res,l,r) \
  (*(res) = ((l) > (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_gt_integer2(res,l,r) \
  (*(res) = ((l) > (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_gt_integer3(res,l,r) \
  (*(res) = ((l) > (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_gt_integer4(res,l,r) \
  (*(res) = ((l) > (r)) ? TRUE : FALSE, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_gt_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     *(res) = (REAL_VALUES_LESS (lr, rr) || REAL_VALUES_EQUAL (lr, rr)) \
       ? FALSE : TRUE; \
     FFEBAD; })
#define ffetarget_gt_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     *(res) = (REAL_VALUES_LESS (lr, rr) || REAL_VALUES_EQUAL (lr, rr)) \
       ? FALSE : TRUE; \
     FFEBAD; })
#else
#define ffetarget_gt_real1(res,l,r) \
  (*(res) = ((l) > (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_gt_real2(res,l,r) \
  (*(res) = ((l) > (r)) ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_hexxmil(v,t) ffetarget_typeless_hex (v, t)
#define ffetarget_hexxvxt(v,t) ffetarget_typeless_hex (v, t)
#define ffetarget_hexzmil(v,t) ffetarget_typeless_hex (v, t)
#define ffetarget_hexzvxt(v,t) ffetarget_typeless_hex (v, t)
#define ffetarget_init_0()
#define ffetarget_init_1()
#define ffetarget_init_2()
#define ffetarget_init_3()
#define ffetarget_init_4()
#ifdef FFETARGET_32bit_longs
#define ffetarget_integerdefault_is_magical(i) \
  (((unsigned long int) i) == FFETARGET_integerBIG_MAGICAL)
#else
#define ffetarget_integerdefault_is_magical(i) \
  (((unsigned int) i) == FFETARGET_integerBIG_MAGICAL)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_iszero_real1(l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     REAL_VALUES_EQUAL (lr, dconst0); \
   })
#define ffetarget_iszero_real2(l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     REAL_VALUES_EQUAL (lr, dconst0); \
   })
#else
#define ffetarget_iszero_real1(l) ((l) == 0.)
#define ffetarget_iszero_real2(l) ((l) == 0.)
#endif
#define ffetarget_iszero_typeless(l) ((l) == 0)
#define ffetarget_logical1(v,truth) (*(v) = truth ? 1 : 0)
#define ffetarget_le_integer1(res,l,r) \
  (*(res) = ((l) <= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_le_integer2(res,l,r) \
  (*(res) = ((l) <= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_le_integer3(res,l,r) \
  (*(res) = ((l) <= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_le_integer4(res,l,r) \
  (*(res) = ((l) <= (r)) ? TRUE : FALSE, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_le_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     *(res) = (REAL_VALUES_LESS (lr, rr) || REAL_VALUES_EQUAL (lr, rr)) \
       ? TRUE : FALSE; \
     FFEBAD; })
#define ffetarget_le_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     *(res) = (REAL_VALUES_LESS (lr, rr) || REAL_VALUES_EQUAL (lr, rr)) \
       ? TRUE : FALSE; \
     FFEBAD; })
#else
#define ffetarget_le_real1(res,l,r) \
  (*(res) = ((l) <= (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_le_real2(res,l,r) \
  (*(res) = ((l) <= (r)) ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_lt_integer1(res,l,r) \
  (*(res) = ((l) < (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_lt_integer2(res,l,r) \
  (*(res) = ((l) < (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_lt_integer3(res,l,r) \
  (*(res) = ((l) < (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_lt_integer4(res,l,r) \
  (*(res) = ((l) < (r)) ? TRUE : FALSE, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_lt_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     *(res) = REAL_VALUES_LESS (lr, rr) ? TRUE : FALSE; \
     FFEBAD; })
#define ffetarget_lt_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     *(res) = REAL_VALUES_LESS (lr, rr) ? TRUE : FALSE; \
     FFEBAD; })
#else
#define ffetarget_lt_real1(res,l,r) \
  (*(res) = ((l) < (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_lt_real2(res,l,r) \
  (*(res) = ((l) < (r)) ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_length_character1(c) ((c).length)
#define ffetarget_length_characterdefault ffetarget_length_character1
#ifdef REAL_ARITHMETIC
#define ffetarget_make_real1(res,lr) \
  ffetarget_cvt_rv_to_r1_ ((lr), *(res))
#define ffetarget_make_real2(res,lr) \
  ffetarget_cvt_rv_to_r2_ ((lr), &((res)->v[0]))
#else
#define ffetarget_make_real1(res,lr) (*(res) = (lr))
#define ffetarget_make_real2(res,lr) (*(res) = (lr))
#endif
#define ffetarget_multiply_integer1(res,l,r) (*(res) = (l) * (r), FFEBAD)
#define ffetarget_multiply_integer2(res,l,r) (*(res) = (l) * (r), FFEBAD)
#define ffetarget_multiply_integer3(res,l,r) (*(res) = (l) * (r), FFEBAD)
#define ffetarget_multiply_integer4(res,l,r) (*(res) = (l) * (r), FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_multiply_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     REAL_ARITHMETIC (resr, MULT_EXPR, lr, rr); \
     ffetarget_cvt_rv_to_r1_ (resr, *(res)); \
     FFEBAD; })
#define ffetarget_multiply_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     REAL_ARITHMETIC (resr, MULT_EXPR, lr, rr); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->v[0])); \
     FFEBAD; })
#else
#define ffetarget_multiply_real1(res,l,r) (*(res) = (l) * (r), FFEBAD)
#define ffetarget_multiply_real2(res,l,r) (*(res) = (l) * (r), FFEBAD)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_ne_complex1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     li = ffetarget_cvt_r1_to_rv_ ((l).imaginary); \
     rr = ffetarget_cvt_r1_to_rv_ ((r).real); \
     ri = ffetarget_cvt_r1_to_rv_ ((r).imaginary); \
     *(res) = (REAL_VALUES_EQUAL (lr, rr) && REAL_VALUES_EQUAL (li, ri)) \
       ? FALSE : TRUE; \
     FFEBAD; })
#define ffetarget_ne_complex2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     li = ffetarget_cvt_r2_to_rv_ (&((l).imaginary.v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).real.v[0])); \
     ri = ffetarget_cvt_r2_to_rv_ (&((r).imaginary.v[0])); \
     *(res) = (REAL_VALUES_EQUAL (lr, rr) && REAL_VALUES_EQUAL (li, ri)) \
       ? FALSE : TRUE; \
     FFEBAD; })
#else
#define ffetarget_ne_complex1(res,l,r) \
  (*(res) = (((l).real != (r).real) || ((l).imaginary != (r).imaginary))  \
   ? TRUE : FALSE, FFEBAD)
#define ffetarget_ne_complex2(res,l,r) \
  (*(res) = (((l).real != (r).real) || ((l).imaginary != (r).imaginary))  \
   ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_ne_integer1(res,l,r) \
  (*(res) = ((l) != (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ne_integer2(res,l,r) \
  (*(res) = ((l) != (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ne_integer3(res,l,r) \
  (*(res) = ((l) != (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ne_integer4(res,l,r) \
  (*(res) = ((l) != (r)) ? TRUE : FALSE, FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_ne_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     *(res) = REAL_VALUES_EQUAL (lr, rr) ? FALSE : TRUE; \
     FFEBAD; })
#define ffetarget_ne_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     *(res) = REAL_VALUES_EQUAL (lr, rr) ? FALSE : TRUE; \
     FFEBAD; })
#else
#define ffetarget_ne_real1(res,l,r) \
  (*(res) = ((l) != (r)) ? TRUE : FALSE, FFEBAD)
#define ffetarget_ne_real2(res,l,r) \
  (*(res) = ((l) != (r)) ? TRUE : FALSE, FFEBAD)
#endif
#define ffetarget_neqv_integer1(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_neqv_integer2(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_neqv_integer3(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_neqv_integer4(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_neqv_logical1(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_neqv_logical2(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_neqv_logical3(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_neqv_logical4(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_not_integer1(res,l) (*(res) = ~(l), FFEBAD)
#define ffetarget_not_integer2(res,l) (*(res) = ~(l), FFEBAD)
#define ffetarget_not_integer3(res,l) (*(res) = ~(l), FFEBAD)
#define ffetarget_not_integer4(res,l) (*(res) = ~(l), FFEBAD)
#define ffetarget_not_logical1(res,l) (*(res) = !(l), FFEBAD)
#define ffetarget_not_logical2(res,l) (*(res) = !(l), FFEBAD)
#define ffetarget_not_logical3(res,l) (*(res) = !(l), FFEBAD)
#define ffetarget_not_logical4(res,l) (*(res) = !(l), FFEBAD)
#define ffetarget_octalmil(v,t) ffetarget_typeless_octal (v, t)
#define ffetarget_octalvxt(v,t) ffetarget_typeless_octal (v, t)
#define ffetarget_offset(res,l) (*(res) = (l), TRUE)	/* Overflow? */
#define ffetarget_offset_add(res,l,r) (*(res) = (l) + (r), TRUE)	/* Overflow? */
#define ffetarget_offset_charsize(res,l,u) (*(res) = (l) * (u), TRUE)	/* Ov? */
#define ffetarget_offset_multiply(res,l,r) (*(res) = (l) * (r), TRUE)	/* Ov? */
#define ffetarget_offset_overflow(text) ((void) 0)	/* ~~no message? */
#define ffetarget_or_integer1(res,l,r) (*(res) = (l) | (r), FFEBAD)
#define ffetarget_or_integer2(res,l,r) (*(res) = (l) | (r), FFEBAD)
#define ffetarget_or_integer3(res,l,r) (*(res) = (l) | (r), FFEBAD)
#define ffetarget_or_integer4(res,l,r) (*(res) = (l) | (r), FFEBAD)
#define ffetarget_or_logical1(res,l,r) (*(res) = (l) || (r), FFEBAD)
#define ffetarget_or_logical2(res,l,r) (*(res) = (l) || (r), FFEBAD)
#define ffetarget_or_logical3(res,l,r) (*(res) = (l) || (r), FFEBAD)
#define ffetarget_or_logical4(res,l,r) (*(res) = (l) || (r), FFEBAD)
#define ffetarget_print_binarymil(f,v) ffetarget_print_binary (f, v)
#define ffetarget_print_binaryvxt(f,v) ffetarget_print_binary (f, v)
#define ffetarget_print_hexxmil(f,v) ffetarget_print_hex (f, v)
#define ffetarget_print_hexxvxt(f,v) ffetarget_print_hex (f, v)
#define ffetarget_print_hexzmil(f,v) ffetarget_print_hex (f, v)
#define ffetarget_print_hexzvxt(f,v) ffetarget_print_hex (f, v)
#define ffetarget_print_integer1(f,v) \
  fprintf ((f), "%" ffetargetInteger1_f "d", (v))
#define ffetarget_print_integer2(f,v) \
  fprintf ((f), "%" ffetargetInteger2_f "d", (v))
#define ffetarget_print_integer3(f,v) \
  fprintf ((f), "%" ffetargetInteger3_f "d", (v))
#define ffetarget_print_integer4(f,v) \
  fprintf ((f), "%" ffetargetInteger4_f "d", (v))
#define ffetarget_print_logical1(f,v) \
  fprintf ((f), "%" ffetargetLogical1_f "d", (v))
#define ffetarget_print_logical2(f,v) \
  fprintf ((f), "%" ffetargetLogical2_f "d", (v))
#define ffetarget_print_logical3(f,v) \
  fprintf ((f), "%" ffetargetLogical3_f "d", (v))
#define ffetarget_print_logical4(f,v) \
  fprintf ((f), "%" ffetargetLogical4_f "d", (v))
#define ffetarget_print_octalmil(f,v) ffetarget_print_octal(f,v)
#define ffetarget_print_octalvxt(f,v) ffetarget_print_octal(f,v)
#ifdef REAL_ARITHMETIC
#define ffetarget_print_real1(f,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     REAL_VALUE_TO_DECIMAL (lr, bad_fmt_val??, ffetarget_string_); \
     fputs (ffetarget_string_, (f)); \
   })
#define ffetarget_print_real2(f,l) \
  ({ REAL_VALUE_TYPE lr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     REAL_VALUE_TO_DECIMAL (lr, bad_fmt_val??, ffetarget_string_); \
     fputs (ffetarget_string_, (f)); \
   })
#else
#define ffetarget_print_real1(f,v) \
  fprintf ((f), "%" ffetargetReal1_f "g", (v))
#define ffetarget_print_real2(f,v) \
  fprintf ((f), "%" ffetargetReal2_f "g", (v))
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_real1_one(res) ffetarget_cvt_rv_to_r1_ (dconst1, *(res))
#define ffetarget_real2_one(res) ffetarget_cvt_rv_to_r2_ (dconst1, &((res)->v[0]))
#else
#define ffetarget_real1_one(res) (*(res) = (float) 1.)
#define ffetarget_real2_one(res) (*(res) = 1.)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_real1_two(res) ffetarget_cvt_rv_to_r1_ (dconst2, *(res))
#define ffetarget_real2_two(res) ffetarget_cvt_rv_to_r2_ (dconst2, &((res)->v[0]))
#else
#define ffetarget_real1_two(res) (*(res) = (float) 2.)
#define ffetarget_real2_two(res) (*(res) = 2.)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_real1_zero(res) ffetarget_cvt_rv_to_r1_ (dconst0, *(res))
#define ffetarget_real2_zero(res) ffetarget_cvt_rv_to_r2_ (dconst0, &((res)->v[0]))
#else
#define ffetarget_real1_zero(res) (*(res) = (float) 0.)
#define ffetarget_real2_zero(res) (*(res) = 0.)
#endif
#define ffetarget_size_typeless_binary(t) ((ffetarget_num_digits_(t) + 7) / 8)
#define ffetarget_size_typeless_octal(t) \
  ((ffetarget_num_digits_(t) * 3 + 7) / 8)
#define ffetarget_size_typeless_hex(t) ((ffetarget_num_digits_(t) + 1) / 2)
#ifdef REAL_ARITHMETIC
#define ffetarget_subtract_complex1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri, resr, resi; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     li = ffetarget_cvt_r1_to_rv_ ((l).imaginary); \
     rr = ffetarget_cvt_r1_to_rv_ ((r).real); \
     ri = ffetarget_cvt_r1_to_rv_ ((r).imaginary); \
     REAL_ARITHMETIC (resr, MINUS_EXPR, lr, rr); \
     REAL_ARITHMETIC (resi, MINUS_EXPR, li, ri); \
     ffetarget_cvt_rv_to_r1_ (resr, (res)->real); \
     ffetarget_cvt_rv_to_r1_ (resi, (res)->imaginary); \
     FFEBAD; })
#define ffetarget_subtract_complex2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, li, rr, ri, resr, resi; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     li = ffetarget_cvt_r2_to_rv_ (&((l).imaginary.v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).real.v[0])); \
     ri = ffetarget_cvt_r2_to_rv_ (&((r).imaginary.v[0])); \
     REAL_ARITHMETIC (resr, MINUS_EXPR, lr, rr); \
     REAL_ARITHMETIC (resi, MINUS_EXPR, li, ri); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->real.v[0])); \
     ffetarget_cvt_rv_to_r2_ (resi, &((res)->imaginary.v[0])); \
     FFEBAD; })
#else
#define ffetarget_subtract_complex1(res,l,r) \
  ((res)->real = (l).real - (r).real, \
   (res)->imaginary = (l).imaginary - (r).imaginary, FFEBAD)
#define ffetarget_subtract_complex2(res,l,r) \
  ((res)->real = (l).real - (r).real, \
   (res)->imaginary = (l).imaginary - (r).imaginary, FFEBAD)
#endif
#define ffetarget_subtract_integer1(res,l,r) (*(res) = (l) - (r), FFEBAD)
#define ffetarget_subtract_integer2(res,l,r) (*(res) = (l) - (r), FFEBAD)
#define ffetarget_subtract_integer3(res,l,r) (*(res) = (l) - (r), FFEBAD)
#define ffetarget_subtract_integer4(res,l,r) (*(res) = (l) - (r), FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_subtract_real1(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     rr = ffetarget_cvt_r1_to_rv_ ((r)); \
     REAL_ARITHMETIC (resr, MINUS_EXPR, lr, rr); \
     ffetarget_cvt_rv_to_r1_ (resr, *(res)); \
     FFEBAD; })
#define ffetarget_subtract_real2(res,l,r) \
  ({ REAL_VALUE_TYPE lr, rr, resr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     rr = ffetarget_cvt_r2_to_rv_ (&((r).v[0])); \
     REAL_ARITHMETIC (resr, MINUS_EXPR, lr, rr); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->v[0])); \
     FFEBAD; })
#else
#define ffetarget_subtract_real1(res,l,r) (*(res) = (l) - (r), FFEBAD)
#define ffetarget_subtract_real2(res,l,r) (*(res) = (l) - (r), FFEBAD)
#endif
#define ffetarget_terminate_0()
#define ffetarget_terminate_1()
#define ffetarget_terminate_2()
#define ffetarget_terminate_3()
#define ffetarget_terminate_4()
#define ffetarget_text_character1(c) ((c).text)
#define ffetarget_text_characterdefault ffetarget_text_character1
#ifdef REAL_ARITHMETIC
#define ffetarget_uminus_complex1(res,l) \
  ({ REAL_VALUE_TYPE lr, li, resr, resi; \
     lr = ffetarget_cvt_r1_to_rv_ ((l).real); \
     li = ffetarget_cvt_r1_to_rv_ ((l).imaginary); \
     resr = REAL_VALUE_NEGATE (lr); \
     resi = REAL_VALUE_NEGATE (li); \
     ffetarget_cvt_rv_to_r1_ (resr, (res)->real); \
     ffetarget_cvt_rv_to_r1_ (resi, (res)->imaginary); \
     FFEBAD; })
#define ffetarget_uminus_complex2(res,l) \
  ({ REAL_VALUE_TYPE lr, li, resr, resi; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).real.v[0])); \
     li = ffetarget_cvt_r2_to_rv_ (&((l).imaginary.v[0])); \
     resr = REAL_VALUE_NEGATE (lr); \
     resi = REAL_VALUE_NEGATE (li); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->real.v[0])); \
     ffetarget_cvt_rv_to_r2_ (resi, &((res)->imaginary.v[0])); \
     FFEBAD; })
#else
#define ffetarget_uminus_complex1(res,l) \
  ((res)->real = -(l).real, (res)->imaginary = -(l).imaginary, FFEBAD)
#define ffetarget_uminus_complex2(res,l) \
  ((res)->real = -(l).real, (res)->imaginary = -(l).imaginary, FFEBAD)
#endif
#define ffetarget_uminus_integer1(res,l) (*(res) = -(l), FFEBAD)
#define ffetarget_uminus_integer2(res,l) (*(res) = -(l), FFEBAD)
#define ffetarget_uminus_integer3(res,l) (*(res) = -(l), FFEBAD)
#define ffetarget_uminus_integer4(res,l) (*(res) = -(l), FFEBAD)
#ifdef REAL_ARITHMETIC
#define ffetarget_uminus_real1(res,l) \
  ({ REAL_VALUE_TYPE lr, resr; \
     lr = ffetarget_cvt_r1_to_rv_ ((l)); \
     resr = REAL_VALUE_NEGATE (lr); \
     ffetarget_cvt_rv_to_r1_ (resr, *(res)); \
     FFEBAD; })
#define ffetarget_uminus_real2(res,l) \
  ({ REAL_VALUE_TYPE lr, resr; \
     lr = ffetarget_cvt_r2_to_rv_ (&((l).v[0])); \
     resr = REAL_VALUE_NEGATE (lr); \
     ffetarget_cvt_rv_to_r2_ (resr, &((res)->v[0])); \
     FFEBAD; })
#else
#define ffetarget_uminus_real1(res,l) (*(res) = -(l), FFEBAD)
#define ffetarget_uminus_real2(res,l) (*(res) = -(l), FFEBAD)
#endif
#ifdef REAL_ARITHMETIC
#define ffetarget_value_real1(lr) ffetarget_cvt_r1_to_rv_ ((lr))
#define ffetarget_value_real2(lr) ffetarget_cvt_r2_to_rv_ (&((lr).v[0]))
#else
#define ffetarget_value_real1
#define ffetarget_value_real2
#endif
#define ffetarget_xor_integer1(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_xor_integer2(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_xor_integer3(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_xor_integer4(res,l,r) (*(res) = (l) ^ (r), FFEBAD)
#define ffetarget_xor_logical1(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_xor_logical2(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_xor_logical3(res,l,r) (*(res) = (l) != (r), FFEBAD)
#define ffetarget_xor_logical4(res,l,r) (*(res) = (l) != (r), FFEBAD)

/* End of #include file. */

#endif
