/* bld.h -- Public #include File (module.h template V1.0)
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
      bld.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_BLD_H
#define GCC_F_BLD_H

/* Simple definitions and enumerations. */

typedef enum
  {
    FFEBLD_constNONE,
    FFEBLD_constINTEGER1,
    FFEBLD_constINTEGER2,
    FFEBLD_constINTEGER3,
    FFEBLD_constINTEGER4,
    FFEBLD_constINTEGER5,
    FFEBLD_constINTEGER6,
    FFEBLD_constINTEGER7,
    FFEBLD_constINTEGER8,
    FFEBLD_constLOGICAL1,
    FFEBLD_constLOGICAL2,
    FFEBLD_constLOGICAL3,
    FFEBLD_constLOGICAL4,
    FFEBLD_constLOGICAL5,
    FFEBLD_constLOGICAL6,
    FFEBLD_constLOGICAL7,
    FFEBLD_constLOGICAL8,
    FFEBLD_constREAL1,
    FFEBLD_constREAL2,
    FFEBLD_constREAL3,
    FFEBLD_constREAL4,
    FFEBLD_constREAL5,
    FFEBLD_constREAL6,
    FFEBLD_constREAL7,
    FFEBLD_constREAL8,
    FFEBLD_constCOMPLEX1,
    FFEBLD_constCOMPLEX2,
    FFEBLD_constCOMPLEX3,
    FFEBLD_constCOMPLEX4,
    FFEBLD_constCOMPLEX5,
    FFEBLD_constCOMPLEX6,
    FFEBLD_constCOMPLEX7,
    FFEBLD_constCOMPLEX8,
    FFEBLD_constCHARACTER1,
    FFEBLD_constCHARACTER2,
    FFEBLD_constCHARACTER3,
    FFEBLD_constCHARACTER4,
    FFEBLD_constCHARACTER5,
    FFEBLD_constCHARACTER6,
    FFEBLD_constCHARACTER7,
    FFEBLD_constCHARACTER8,
    FFEBLD_constHOLLERITH,
    FFEBLD_constTYPELESS_FIRST,
    FFEBLD_constBINARY_MIL = FFEBLD_constTYPELESS_FIRST,
    FFEBLD_constBINARY_VXT,
    FFEBLD_constOCTAL_MIL,
    FFEBLD_constOCTAL_VXT,
    FFEBLD_constHEX_X_MIL,
    FFEBLD_constHEX_X_VXT,
    FFEBLD_constHEX_Z_MIL,
    FFEBLD_constHEX_Z_VXT,
    FFEBLD_constTYPELESS_LAST = FFEBLD_constHEX_Z_VXT,
    FFEBLD_const
  } ffebldConst;

typedef enum
  {
#define FFEBLD_OP(KWD,NAME,ARITY) KWD,
#include "bld-op.def"
#undef FFEBLD_OP
    FFEBLD_op
  } ffebldOp;

/* Typedefs. */

typedef struct _ffebld_ *ffebld;
typedef unsigned char ffebldArity;
typedef union _ffebld_constant_array_ ffebldConstantArray;
typedef struct _ffebld_constant_ *ffebldConstant;
typedef union _ffebld_constant_union_ ffebldConstantUnion;
typedef ffebld *ffebldListBottom;
typedef unsigned int ffebldListLength;
#define ffebldListLength_f ""
typedef struct _ffebld_pool_stack_ *ffebldPoolstack_;

/* Include files needed by this one. */

#include "bit.h"
#include "com.h"
#include "info.h"
#include "intrin.h"
#include "lab.h"
#include "lex.h"
#include "malloc.h"
#include "symbol.h"
#include "target.h"

#define FFEBLD_whereconstPROGUNIT_ 1
#define FFEBLD_whereconstFILE_ 2

#define FFEBLD_whereconstCURRENT_ FFEBLD_whereconstFILE_

/* Structure definitions. */

#define FFEBLD_constINTEGERDEFAULT FFEBLD_constINTEGER1
#define FFEBLD_constLOGICALDEFAULT FFEBLD_constLOGICAL1
#define FFEBLD_constREALDEFAULT FFEBLD_constREAL1
#define FFEBLD_constREALDOUBLE FFEBLD_constREAL2
#define FFEBLD_constREALQUAD FFEBLD_constREAL3
#define FFEBLD_constCOMPLEX FFEBLD_constCOMPLEX1
#define FFEBLD_constCOMPLEXDOUBLE FFEBLD_constCOMPLEX2
#define FFEBLD_constCOMPLEXQUAD FFEBLD_constCOMPLEX3
#define FFEBLD_constCHARACTERDEFAULT FFEBLD_constCHARACTER1

union _ffebld_constant_union_
  {
    ffetargetTypeless typeless;
    ffetargetHollerith hollerith;
#if FFETARGET_okINTEGER1
    ffetargetInteger1 integer1;
#endif
#if FFETARGET_okINTEGER2
    ffetargetInteger2 integer2;
#endif
#if FFETARGET_okINTEGER3
    ffetargetInteger3 integer3;
#endif
#if FFETARGET_okINTEGER4
    ffetargetInteger4 integer4;
#endif
#if FFETARGET_okINTEGER5
    ffetargetInteger5 integer5;
#endif
#if FFETARGET_okINTEGER6
    ffetargetInteger6 integer6;
#endif
#if FFETARGET_okINTEGER7
    ffetargetInteger7 integer7;
#endif
#if FFETARGET_okINTEGER8
    ffetargetInteger8 integer8;
#endif
#if FFETARGET_okLOGICAL1
    ffetargetLogical1 logical1;
#endif
#if FFETARGET_okLOGICAL2
    ffetargetLogical2 logical2;
#endif
#if FFETARGET_okLOGICAL3
    ffetargetLogical3 logical3;
#endif
#if FFETARGET_okLOGICAL4
    ffetargetLogical4 logical4;
#endif
#if FFETARGET_okLOGICAL5
    ffetargetLogical5 logical5;
#endif
#if FFETARGET_okLOGICAL6
    ffetargetLogical6 logical6;
#endif
#if FFETARGET_okLOGICAL7
    ffetargetLogical7 logical7;
#endif
#if FFETARGET_okLOGICAL8
    ffetargetLogical8 logical8;
#endif
#if FFETARGET_okREAL1
    ffetargetReal1 real1;
#endif
#if FFETARGET_okREAL2
    ffetargetReal2 real2;
#endif
#if FFETARGET_okREAL3
    ffetargetReal3 real3;
#endif
#if FFETARGET_okREAL4
    ffetargetReal4 real4;
#endif
#if FFETARGET_okREAL5
    ffetargetReal5 real5;
#endif
#if FFETARGET_okREAL6
    ffetargetReal6 real6;
#endif
#if FFETARGET_okREAL7
    ffetargetReal7 real7;
#endif
#if FFETARGET_okREAL8
    ffetargetReal8 real8;
#endif
#if FFETARGET_okCOMPLEX1
    ffetargetComplex1 complex1;
#endif
#if FFETARGET_okCOMPLEX2
    ffetargetComplex2 complex2;
#endif
#if FFETARGET_okCOMPLEX3
    ffetargetComplex3 complex3;
#endif
#if FFETARGET_okCOMPLEX4
    ffetargetComplex4 complex4;
#endif
#if FFETARGET_okCOMPLEX5
    ffetargetComplex5 complex5;
#endif
#if FFETARGET_okCOMPLEX6
    ffetargetComplex6 complex6;
#endif
#if FFETARGET_okCOMPLEX7
    ffetargetComplex7 complex7;
#endif
#if FFETARGET_okCOMPLEX8
    ffetargetComplex8 complex8;
#endif
#if FFETARGET_okCHARACTER1
    ffetargetCharacter1 character1;
#endif
#if FFETARGET_okCHARACTER2
    ffetargetCharacter2 character2;
#endif
#if FFETARGET_okCHARACTER3
    ffetargetCharacter3 character3;
#endif
#if FFETARGET_okCHARACTER4
    ffetargetCharacter4 character4;
#endif
#if FFETARGET_okCHARACTER5
    ffetargetCharacter5 character5;
#endif
#if FFETARGET_okCHARACTER6
    ffetargetCharacter6 character6;
#endif
#if FFETARGET_okCHARACTER7
    ffetargetCharacter7 character7;
#endif
#if FFETARGET_okCHARACTER8
    ffetargetCharacter8 character8;
#endif
  };

union _ffebld_constant_array_
  {
#if FFETARGET_okINTEGER1
    ffetargetInteger1 *integer1;
#endif
#if FFETARGET_okINTEGER2
    ffetargetInteger2 *integer2;
#endif
#if FFETARGET_okINTEGER3
    ffetargetInteger3 *integer3;
#endif
#if FFETARGET_okINTEGER4
    ffetargetInteger4 *integer4;
#endif
#if FFETARGET_okINTEGER5
    ffetargetInteger5 *integer5;
#endif
#if FFETARGET_okINTEGER6
    ffetargetInteger6 *integer6;
#endif
#if FFETARGET_okINTEGER7
    ffetargetInteger7 *integer7;
#endif
#if FFETARGET_okINTEGER8
    ffetargetInteger8 *integer8;
#endif
#if FFETARGET_okLOGICAL1
    ffetargetLogical1 *logical1;
#endif
#if FFETARGET_okLOGICAL2
    ffetargetLogical2 *logical2;
#endif
#if FFETARGET_okLOGICAL3
    ffetargetLogical3 *logical3;
#endif
#if FFETARGET_okLOGICAL4
    ffetargetLogical4 *logical4;
#endif
#if FFETARGET_okLOGICAL5
    ffetargetLogical5 *logical5;
#endif
#if FFETARGET_okLOGICAL6
    ffetargetLogical6 *logical6;
#endif
#if FFETARGET_okLOGICAL7
    ffetargetLogical7 *logical7;
#endif
#if FFETARGET_okLOGICAL8
    ffetargetLogical8 *logical8;
#endif
#if FFETARGET_okREAL1
    ffetargetReal1 *real1;
#endif
#if FFETARGET_okREAL2
    ffetargetReal2 *real2;
#endif
#if FFETARGET_okREAL3
    ffetargetReal3 *real3;
#endif
#if FFETARGET_okREAL4
    ffetargetReal4 *real4;
#endif
#if FFETARGET_okREAL5
    ffetargetReal5 *real5;
#endif
#if FFETARGET_okREAL6
    ffetargetReal6 *real6;
#endif
#if FFETARGET_okREAL7
    ffetargetReal7 *real7;
#endif
#if FFETARGET_okREAL8
    ffetargetReal8 *real8;
#endif
#if FFETARGET_okCOMPLEX1
    ffetargetComplex1 *complex1;
#endif
#if FFETARGET_okCOMPLEX2
    ffetargetComplex2 *complex2;
#endif
#if FFETARGET_okCOMPLEX3
    ffetargetComplex3 *complex3;
#endif
#if FFETARGET_okCOMPLEX4
    ffetargetComplex4 *complex4;
#endif
#if FFETARGET_okCOMPLEX5
    ffetargetComplex5 *complex5;
#endif
#if FFETARGET_okCOMPLEX6
    ffetargetComplex6 *complex6;
#endif
#if FFETARGET_okCOMPLEX7
    ffetargetComplex7 *complex7;
#endif
#if FFETARGET_okCOMPLEX8
    ffetargetComplex8 *complex8;
#endif
#if FFETARGET_okCHARACTER1
    ffetargetCharacterUnit1 *character1;
#endif
#if FFETARGET_okCHARACTER2
    ffetargetCharacterUnit2 *character2;
#endif
#if FFETARGET_okCHARACTER3
    ffetargetCharacterUnit3 *character3;
#endif
#if FFETARGET_okCHARACTER4
    ffetargetCharacterUnit4 *character4;
#endif
#if FFETARGET_okCHARACTER5
    ffetargetCharacterUnit5 *character5;
#endif
#if FFETARGET_okCHARACTER6
    ffetargetCharacterUnit6 *character6;
#endif
#if FFETARGET_okCHARACTER7
    ffetargetCharacterUnit7 *character7;
#endif
#if FFETARGET_okCHARACTER8
    ffetargetCharacterUnit8 *character8;
#endif
  };

struct _ffebld_
  {
    ffebldOp op;
    ffeinfo info;		/* Not used or valid for
				   op=={STAR,ITEM,BOUNDS,REPEAT,LABTER,
				   LABTOK,IMPDO}. */
    union
      {
	struct
	  {
	    ffebld left;
	    ffebld right;
#ifdef FFECOM_nonterHOOK
	    ffecomNonter hook;	/* Whatever the compiler/backend wants! */
#endif
	  }
	nonter;
	struct
	  {
	    ffebld head;
	    ffebld trail;
#ifdef FFECOM_itemHOOK
	    ffecomItem hook;	/* Whatever the compiler/backend wants! */
#endif
	  }
	item;
	struct
	  {
	    ffebldConstant expr;
	    ffebld orig;	/* Original expression, or NULL if none. */
	    ffetargetAlign pad;	/* Initial padding (for DATA, etc.). */
	  }
	conter;
	struct
	  {
	    ffebldConstantArray array;
	    ffetargetOffset size;
	    ffetargetAlign pad;	/* Initial padding (for DATA, etc.). */
	  }
	arrter;
	struct
	  {
	    ffebldConstantArray array;
	    ffebit bits;
	    ffetargetAlign pad;	/* Initial padding (for DATA, etc.). */
	  }
	accter;
	struct
	  {
	    ffesymbol symbol;
	    ffeintrinGen generic;	/* Id for generic intrinsic. */
	    ffeintrinSpec specific;	/* Id for specific intrinsic. */
	    ffeintrinImp implementation;	/* Id for implementation. */
	    bool do_iter;	/* TRUE if this ref is a read-only ref by
				   definition (ref within DO loop using this
				   var as iterator). */
	  }
	symter;
	ffelab labter;
	ffelexToken labtok;
      }
    u;
  };

struct _ffebld_constant_
  {
    ffebldConstant next;
    ffebldConstant first_complex;	/* First complex const with me as
					   real. */
    ffebldConstant negated;	/* We point to each other through here. */
    ffebldConst consttype;
#ifdef FFECOM_constantHOOK
    ffecomConstant hook;	/* Whatever the compiler/backend wants! */
#endif
    bool numeric;		/* A numeric kind of constant. */
    ffebldConstantUnion u;
  };

struct _ffebld_pool_stack_
  {
    ffebldPoolstack_ next;
    mallocPool pool;
  };

/* Global objects accessed by users of this module. */

extern const ffebldArity ffebld_arity_op_[(int) FFEBLD_op];
extern struct _ffebld_pool_stack_ ffebld_pool_stack_;

/* Declare functions with prototypes. */

int ffebld_constant_cmp (ffebldConstant c1, ffebldConstant c2);
bool ffebld_constant_is_magical (ffebldConstant c);
bool ffebld_constant_is_zero (ffebldConstant c);
#if FFETARGET_okCHARACTER1
ffebldConstant ffebld_constant_new_character1 (ffelexToken t);
ffebldConstant ffebld_constant_new_character1_val (ffetargetCharacter1 val);
#endif
#if FFETARGET_okCHARACTER2
ffebldConstant ffebld_constant_new_character2 (ffelexToken t);
ffebldConstant ffebld_constant_new_character2_val (ffetargetCharacter2 val);
#endif
#if FFETARGET_okCHARACTER3
ffebldConstant ffebld_constant_new_character3 (ffelexToken t);
ffebldConstant ffebld_constant_new_character3_val (ffetargetCharacter3 val);
#endif
#if FFETARGET_okCHARACTER4
ffebldConstant ffebld_constant_new_character4 (ffelexToken t);
ffebldConstant ffebld_constant_new_character4_val (ffetargetCharacter4 val);
#endif
#if FFETARGET_okCHARACTER5
ffebldConstant ffebld_constant_new_character5 (ffelexToken t);
ffebldConstant ffebld_constant_new_character5_val (ffetargetCharacter5 val);
#endif
#if FFETARGET_okCHARACTER6
ffebldConstant ffebld_constant_new_character6 (ffelexToken t);
ffebldConstant ffebld_constant_new_character6_val (ffetargetCharacter6 val);
#endif
#if FFETARGET_okCHARACTER7
ffebldConstant ffebld_constant_new_character7 (ffelexToken t);
ffebldConstant ffebld_constant_new_character7_val (ffetargetCharacter7 val);
#endif
#if FFETARGET_okCHARACTER8
ffebldConstant ffebld_constant_new_character8 (ffelexToken t);
ffebldConstant ffebld_constant_new_character8_val (ffetargetCharacter8 val);
#endif
#if FFETARGET_okCOMPLEX1
ffebldConstant ffebld_constant_new_complex1 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex1_val (ffetargetComplex1 val);
#endif
#if FFETARGET_okCOMPLEX2
ffebldConstant ffebld_constant_new_complex2 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex2_val (ffetargetComplex2 val);
#endif
#if FFETARGET_okCOMPLEX3
ffebldConstant ffebld_constant_new_complex3 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex3_val (ffetargetComplex3 val);
#endif
#if FFETARGET_okCOMPLEX4
ffebldConstant ffebld_constant_new_complex4 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex4_val (ffetargetComplex4 val);
#endif
#if FFETARGET_okCOMPLEX5
ffebldConstant ffebld_constant_new_complex5 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex5_val (ffetargetComplex5 val);
#endif
#if FFETARGET_okCOMPLEX6
ffebldConstant ffebld_constant_new_complex6 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex6_val (ffetargetComplex6 val);
#endif
#if FFETARGET_okCOMPLEX7
ffebldConstant ffebld_constant_new_complex7 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex7_val (ffetargetComplex7 val);
#endif
#if FFETARGET_okCOMPLEX8
ffebldConstant ffebld_constant_new_complex8 (ffebldConstant real,
					     ffebldConstant imaginary);
ffebldConstant ffebld_constant_new_complex8_val (ffetargetComplex8 val);
#endif
ffebldConstant ffebld_constant_new_hollerith (ffelexToken t);
ffebldConstant ffebld_constant_new_hollerith_val (ffetargetHollerith val);
#if FFETARGET_okINTEGER1
ffebldConstant ffebld_constant_new_integer1 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer1_val (ffetargetInteger1 val);
#endif
#if FFETARGET_okINTEGER2
ffebldConstant ffebld_constant_new_integer2 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer2_val (ffetargetInteger2 val);
#endif
#if FFETARGET_okINTEGER3
ffebldConstant ffebld_constant_new_integer3 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer3_val (ffetargetInteger3 val);
#endif
#if FFETARGET_okINTEGER4
ffebldConstant ffebld_constant_new_integer4 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer4_val (ffetargetInteger4 val);
#endif
#if FFETARGET_okINTEGER5
ffebldConstant ffebld_constant_new_integer5 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer5_val (ffetargetInteger5 val);
#endif
#if FFETARGET_okINTEGER6
ffebldConstant ffebld_constant_new_integer6 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer6_val (ffetargetInteger6 val);
#endif
#if FFETARGET_okINTEGER7
ffebldConstant ffebld_constant_new_integer7 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer7_val (ffetargetInteger7 val);
#endif
#if FFETARGET_okINTEGER8
ffebldConstant ffebld_constant_new_integer8 (ffelexToken t);
ffebldConstant ffebld_constant_new_integer8_val (ffetargetInteger8 val);
#endif
ffebldConstant ffebld_constant_new_integerbinary (ffelexToken t);
ffebldConstant ffebld_constant_new_integerhex (ffelexToken t);
ffebldConstant ffebld_constant_new_integeroctal (ffelexToken t);
#if FFETARGET_okLOGICAL1
ffebldConstant ffebld_constant_new_logical1 (bool truth);
ffebldConstant ffebld_constant_new_logical1_val (ffetargetLogical1 val);
#endif
#if FFETARGET_okLOGICAL2
ffebldConstant ffebld_constant_new_logical2 (bool truth);
ffebldConstant ffebld_constant_new_logical2_val (ffetargetLogical2 val);
#endif
#if FFETARGET_okLOGICAL3
ffebldConstant ffebld_constant_new_logical3 (bool truth);
ffebldConstant ffebld_constant_new_logical3_val (ffetargetLogical3 val);
#endif
#if FFETARGET_okLOGICAL4
ffebldConstant ffebld_constant_new_logical4 (bool truth);
ffebldConstant ffebld_constant_new_logical4_val (ffetargetLogical4 val);
#endif
#if FFETARGET_okLOGICAL5
ffebldConstant ffebld_constant_new_logical5 (bool truth);
ffebldConstant ffebld_constant_new_logical5_val (ffetargetLogical5 val);
#endif
#if FFETARGET_okLOGICAL6
ffebldConstant ffebld_constant_new_logical6 (bool truth);
ffebldConstant ffebld_constant_new_logical6_val (ffetargetLogical6 val);
#endif
#if FFETARGET_okLOGICAL7
ffebldConstant ffebld_constant_new_logical7 (bool truth);
ffebldConstant ffebld_constant_new_logical7_val (ffetargetLogical7 val);
#endif
#if FFETARGET_okLOGICAL8
ffebldConstant ffebld_constant_new_logical8 (bool truth);
ffebldConstant ffebld_constant_new_logical8_val (ffetargetLogical8 val);
#endif
#if FFETARGET_okREAL1
ffebldConstant ffebld_constant_new_real1 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real1_val (ffetargetReal1 val);
#endif
#if FFETARGET_okREAL2
ffebldConstant ffebld_constant_new_real2 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real2_val (ffetargetReal2 val);
#endif
#if FFETARGET_okREAL3
ffebldConstant ffebld_constant_new_real3 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real3_val (ffetargetReal3 val);
#endif
#if FFETARGET_okREAL4
ffebldConstant ffebld_constant_new_real4 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real4_val (ffetargetReal4 val);
#endif
#if FFETARGET_okREAL5
ffebldConstant ffebld_constant_new_real5 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real5_val (ffetargetReal5 val);
#endif
#if FFETARGET_okREAL6
ffebldConstant ffebld_constant_new_real6 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real6_val (ffetargetReal6 val);
#endif
#if FFETARGET_okREAL7
ffebldConstant ffebld_constant_new_real7 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real7_val (ffetargetReal7 val);
#endif
#if FFETARGET_okREAL8
ffebldConstant ffebld_constant_new_real8 (ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
ffebldConstant ffebld_constant_new_real8_val (ffetargetReal8 val);
#endif
ffebldConstant ffebld_constant_new_typeless_bm (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_bv (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_hxm (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_hxv (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_hzm (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_hzv (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_om (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_ov (ffelexToken t);
ffebldConstant ffebld_constant_new_typeless_val (ffebldConst type,
						 ffetargetTypeless val);
ffebldConstant ffebld_constant_negated (ffebldConstant c);
ffebldConstantUnion ffebld_constantarray_get (ffebldConstantArray array,
	   ffeinfoBasictype bt, ffeinfoKindtype kt, ffetargetOffset offset);
void ffebld_constantarray_kill (ffebldConstantArray array, ffeinfoBasictype bt,
				ffeinfoKindtype kt, ffetargetOffset size);
ffebldConstantArray ffebld_constantarray_new (ffeinfoBasictype bt,
				  ffeinfoKindtype kt, ffetargetOffset size);
void ffebld_constantarray_prepare (void **aptr, void **cptr, size_t *size,
       ffebldConstantArray array, ffeinfoBasictype abt, ffeinfoKindtype akt,
		      ffetargetOffset offset, ffebldConstantUnion *constant,
				 ffeinfoBasictype cbt, ffeinfoKindtype ckt);
void ffebld_constantarray_preparray (void **aptr, void **cptr, size_t *size,
       ffebldConstantArray array, ffeinfoBasictype abt, ffeinfoKindtype akt,
		   ffetargetOffset offset, ffebldConstantArray source_array,
				 ffeinfoBasictype cbt, ffeinfoKindtype ckt);
void ffebld_constantarray_put (ffebldConstantArray array, ffeinfoBasictype bt,
  ffeinfoKindtype kt, ffetargetOffset offset, ffebldConstantUnion constant);
void ffebld_init_0 (void);
void ffebld_init_1 (void);
void ffebld_init_2 (void);
ffebldListLength ffebld_list_length (ffebld l);
ffebld ffebld_new_accter (ffebldConstantArray array, ffebit b);
ffebld ffebld_new_arrter (ffebldConstantArray array, ffetargetOffset size);
ffebld ffebld_new_conter_with_orig (ffebldConstant c, ffebld orig);
ffebld ffebld_new_item (ffebld head, ffebld trail);
ffebld ffebld_new_labter (ffelab l);
ffebld ffebld_new_labtok (ffelexToken t);
ffebld ffebld_new_none (ffebldOp o);
ffebld ffebld_new_symter (ffesymbol s, ffeintrinGen gen, ffeintrinSpec spec,
			  ffeintrinImp imp);
ffebld ffebld_new_one (ffebldOp o, ffebld left);
ffebld ffebld_new_two (ffebldOp o, ffebld left, ffebld right);
const char *ffebld_op_string (ffebldOp o);
void ffebld_pool_pop (void);
void ffebld_pool_push (mallocPool pool);
ffetargetCharacterSize ffebld_size_max (ffebld b);

/* Define macros. */

#define ffebld_accter(b) ((b)->u.accter.array)
#define ffebld_accter_bits(b) ((b)->u.accter.bits)
#define ffebld_accter_pad(b) ((b)->u.accter.pad)
#define ffebld_accter_set_bits(b,bt) ((b)->u.accter.bits = (bt))
#define ffebld_accter_set_pad(b,p) ((b)->u.accter.pad = (p))
#define ffebld_accter_size(b) ffebit_size((b)->u.accter.bits)
#define ffebld_append_item(b,i) (**(b) = ffebld_new_item((i),NULL),	      \
				 *(b) = &((**(b))->u.item.trail))
#define ffebld_arity(b) ffebld_arity_op(ffebld_op(b))
#define ffebld_arity_op(o) (ffebld_arity_op_[o])
#define ffebld_arrter(b) ((b)->u.arrter.array)
#define ffebld_arrter_pad(b) ((b)->u.arrter.pad)
#define ffebld_arrter_set_pad(b,p) ((b)->u.arrter.pad = (p))
#define ffebld_arrter_set_size(b,s) ((b)->u.arrter.size = (s))
#define ffebld_arrter_size(b) ((b)->u.arrter.size)
#define ffebld_basictype(b) (ffeinfo_basictype (ffebld_info ((b))))
#if FFEBLD_whereconstCURRENT_ == FFEBLD_whereconstPROGUNIT_
#define ffebld_constant_pool() ffe_pool_program_unit()
#elif FFEBLD_whereconstCURRENT_ == FFEBLD_whereconstFILE_
#define ffebld_constant_pool() ffe_pool_file()
#else
#error
#endif
#define ffebld_constant_character1(c) ((c)->u.character1)
#define ffebld_constant_character2(c) ((c)->u.character2)
#define ffebld_constant_character3(c) ((c)->u.character3)
#define ffebld_constant_character4(c) ((c)->u.character4)
#define ffebld_constant_character5(c) ((c)->u.character5)
#define ffebld_constant_character6(c) ((c)->u.character6)
#define ffebld_constant_character7(c) ((c)->u.character7)
#define ffebld_constant_character8(c) ((c)->u.character8)
#define ffebld_constant_characterdefault ffebld_constant_character1
#define ffebld_constant_complex1(c) ((c)->u.complex1)
#define ffebld_constant_complex2(c) ((c)->u.complex2)
#define ffebld_constant_complex3(c) ((c)->u.complex3)
#define ffebld_constant_complex4(c) ((c)->u.complex4)
#define ffebld_constant_complex5(c) ((c)->u.complex5)
#define ffebld_constant_complex6(c) ((c)->u.complex6)
#define ffebld_constant_complex7(c) ((c)->u.complex7)
#define ffebld_constant_complex8(c) ((c)->u.complex8)
#define ffebld_constant_complexdefault ffebld_constant_complex1
#define ffebld_constant_complexdouble ffebld_constant_complex2
#define ffebld_constant_complexquad ffebld_constant_complex3
#define ffebld_constant_copy(c) (c)
#define ffebld_constant_hollerith(c) ((c)->u.hollerith)
#define ffebld_constant_hook(c) ((c)->hook)
#define ffebld_constant_integer1(c) ((c)->u.integer1)
#define ffebld_constant_integer2(c) ((c)->u.integer2)
#define ffebld_constant_integer3(c) ((c)->u.integer3)
#define ffebld_constant_integer4(c) ((c)->u.integer4)
#define ffebld_constant_integer5(c) ((c)->u.integer5)
#define ffebld_constant_integer6(c) ((c)->u.integer6)
#define ffebld_constant_integer7(c) ((c)->u.integer7)
#define ffebld_constant_integer8(c) ((c)->u.integer8)
#define ffebld_constant_integerdefault ffebld_constant_integer1
#define ffebld_constant_is_numeric(c) ((c)->numeric)
#define ffebld_constant_logical1(c) ((c)->u.logical1)
#define ffebld_constant_logical2(c) ((c)->u.logical2)
#define ffebld_constant_logical3(c) ((c)->u.logical3)
#define ffebld_constant_logical4(c) ((c)->u.logical4)
#define ffebld_constant_logical5(c) ((c)->u.logical5)
#define ffebld_constant_logical6(c) ((c)->u.logical6)
#define ffebld_constant_logical7(c) ((c)->u.logical7)
#define ffebld_constant_logical8(c) ((c)->u.logical8)
#define ffebld_constant_logicaldefault ffebld_constant_logical1
#define ffebld_constant_new_characterdefault ffebld_constant_new_character1
#define ffebld_constant_new_characterdefault_val ffebld_constant_new_character1_val
#define ffebld_constant_new_complexdefault ffebld_constant_new_complex1
#define ffebld_constant_new_complexdefault_val ffebld_constant_new_complex1_val
#define ffebld_constant_new_complexdouble ffebld_constant_new_complex2
#define ffebld_constant_new_complexdouble_val ffebld_constant_new_complex2_val
#define ffebld_constant_new_complexquad ffebld_constant_new_complex3
#define ffebld_constant_new_complexquad_valffebld_constant_new_complex3_val
#define ffebld_constant_new_integerdefault ffebld_constant_new_integer1
#define ffebld_constant_new_integerdefault_val ffebld_constant_new_integer1_val
#define ffebld_constant_new_logicaldefault ffebld_constant_new_logical1
#define ffebld_constant_new_logicaldefault_val ffebld_constant_new_logical1_val
#define ffebld_constant_new_realdefault ffebld_constant_new_real1
#define ffebld_constant_new_realdefault_val ffebld_constant_new_real1_val
#define ffebld_constant_new_realdouble ffebld_constant_new_real2
#define ffebld_constant_new_realdouble_val ffebld_constant_new_real2_val
#define ffebld_constant_new_realquad ffebld_constant_new_real3
#define ffebld_constant_new_realquad_val ffebld_constant_new_real3_val
#define ffebld_constant_ptr_to_union(c) (&(c)->u)
#define ffebld_constant_real1(c) ((c)->u.real1)
#define ffebld_constant_real2(c) ((c)->u.real2)
#define ffebld_constant_real3(c) ((c)->u.real3)
#define ffebld_constant_real4(c) ((c)->u.real4)
#define ffebld_constant_real5(c) ((c)->u.real5)
#define ffebld_constant_real6(c) ((c)->u.real6)
#define ffebld_constant_real7(c) ((c)->u.real7)
#define ffebld_constant_real8(c) ((c)->u.real8)
#define ffebld_constant_realdefault ffebld_constant_real1
#define ffebld_constant_realdouble ffebld_constant_real2
#define ffebld_constant_realquad ffebld_constant_real3
#define ffebld_constant_set_hook(c,h) ((c)->hook = (h))
#define ffebld_constant_set_union(c,un) ((c)->u = (un))
#define ffebld_constant_type(c) ((c)->consttype)
#define ffebld_constant_typeless(c) ((c)->u.typeless)
#define ffebld_constant_union(c) ((c)->u)
#define ffebld_conter(b) ((b)->u.conter.expr)
#define ffebld_conter_orig(b) ((b)->u.conter.orig)
#define ffebld_conter_pad(b) ((b)->u.conter.pad)
#define ffebld_conter_set_orig(b,o) ((b)->u.conter.orig = (o))
#define ffebld_conter_set_pad(b,p) ((b)->u.conter.pad = (p))
#define ffebld_copy(b) (b)	/* ~~~Someday really make a copy. */
#define ffebld_cu_ptr_typeless(u) &(u).typeless
#define ffebld_cu_ptr_hollerith(u) &(u).hollerith
#define ffebld_cu_ptr_integer1(u) &(u).integer1
#define ffebld_cu_ptr_integer2(u) &(u).integer2
#define ffebld_cu_ptr_integer3(u) &(u).integer3
#define ffebld_cu_ptr_integer4(u) &(u).integer4
#define ffebld_cu_ptr_integer5(u) &(u).integer5
#define ffebld_cu_ptr_integer6(u) &(u).integer6
#define ffebld_cu_ptr_integer7(u) &(u).integer7
#define ffebld_cu_ptr_integer8(u) &(u).integer8
#define ffebld_cu_ptr_integerdefault ffebld_cu_ptr_integer1
#define ffebld_cu_ptr_logical1(u) &(u).logical1
#define ffebld_cu_ptr_logical2(u) &(u).logical2
#define ffebld_cu_ptr_logical3(u) &(u).logical3
#define ffebld_cu_ptr_logical4(u) &(u).logical4
#define ffebld_cu_ptr_logical5(u) &(u).logical5
#define ffebld_cu_ptr_logical6(u) &(u).logical6
#define ffebld_cu_ptr_logical7(u) &(u).logical7
#define ffebld_cu_ptr_logical8(u) &(u).logical8
#define ffebld_cu_ptr_logicaldefault ffebld_cu_ptr_logical1
#define ffebld_cu_ptr_real1(u) &(u).real1
#define ffebld_cu_ptr_real2(u) &(u).real2
#define ffebld_cu_ptr_real3(u) &(u).real3
#define ffebld_cu_ptr_real4(u) &(u).real4
#define ffebld_cu_ptr_real5(u) &(u).real5
#define ffebld_cu_ptr_real6(u) &(u).real6
#define ffebld_cu_ptr_real7(u) &(u).real7
#define ffebld_cu_ptr_real8(u) &(u).real8
#define ffebld_cu_ptr_realdefault ffebld_cu_ptr_real1
#define ffebld_cu_ptr_realdouble ffebld_cu_ptr_real2
#define ffebld_cu_ptr_realquad ffebld_cu_ptr_real3
#define ffebld_cu_ptr_complex1(u) &(u).complex1
#define ffebld_cu_ptr_complex2(u) &(u).complex2
#define ffebld_cu_ptr_complex3(u) &(u).complex3
#define ffebld_cu_ptr_complex4(u) &(u).complex4
#define ffebld_cu_ptr_complex5(u) &(u).complex5
#define ffebld_cu_ptr_complex6(u) &(u).complex6
#define ffebld_cu_ptr_complex7(u) &(u).complex7
#define ffebld_cu_ptr_complex8(u) &(u).complex8
#define ffebld_cu_ptr_complexdefault ffebld_cu_ptr_complex1
#define ffebld_cu_ptr_complexdouble ffebld_cu_ptr_complex2
#define ffebld_cu_ptr_complexquad ffebld_cu_ptr_complex3
#define ffebld_cu_ptr_character1(u) &(u).character1
#define ffebld_cu_ptr_character2(u) &(u).character2
#define ffebld_cu_ptr_character3(u) &(u).character3
#define ffebld_cu_ptr_character4(u) &(u).character4
#define ffebld_cu_ptr_character5(u) &(u).character5
#define ffebld_cu_ptr_character6(u) &(u).character6
#define ffebld_cu_ptr_character7(u) &(u).character7
#define ffebld_cu_ptr_character8(u) &(u).character8
#define ffebld_cu_val_typeless(u) (u).typeless
#define ffebld_cu_val_hollerith(u) (u).hollerith
#define ffebld_cu_val_integer1(u) (u).integer1
#define ffebld_cu_val_integer2(u) (u).integer2
#define ffebld_cu_val_integer3(u) (u).integer3
#define ffebld_cu_val_integer4(u) (u).integer4
#define ffebld_cu_val_integer5(u) (u).integer5
#define ffebld_cu_val_integer6(u) (u).integer6
#define ffebld_cu_val_integer7(u) (u).integer7
#define ffebld_cu_val_integer8(u) (u).integer8
#define ffebld_cu_val_integerdefault ffebld_cu_val_integer1
#define ffebld_cu_val_logical1(u) (u).logical1
#define ffebld_cu_val_logical2(u) (u).logical2
#define ffebld_cu_val_logical3(u) (u).logical3
#define ffebld_cu_val_logical4(u) (u).logical4
#define ffebld_cu_val_logical5(u) (u).logical5
#define ffebld_cu_val_logical6(u) (u).logical6
#define ffebld_cu_val_logical7(u) (u).logical7
#define ffebld_cu_val_logical8(u) (u).logical8
#define ffebld_cu_val_logicaldefault ffebld_cu_val_logical
#define ffebld_cu_val_real1(u) (u).real1
#define ffebld_cu_val_real2(u) (u).real2
#define ffebld_cu_val_real3(u) (u).real3
#define ffebld_cu_val_real4(u) (u).real4
#define ffebld_cu_val_real5(u) (u).real5
#define ffebld_cu_val_real6(u) (u).real6
#define ffebld_cu_val_real7(u) (u).real7
#define ffebld_cu_val_real8(u) (u).real8
#define ffebld_cu_val_realdefault ffebld_cu_val_real1
#define ffebld_cu_val_realdouble ffebld_cu_val_real2
#define ffebld_cu_val_realquad ffebld_cu_val_real3
#define ffebld_cu_val_complex1(u) (u).complex1
#define ffebld_cu_val_complex2(u) (u).complex2
#define ffebld_cu_val_complex3(u) (u).complex3
#define ffebld_cu_val_complex4(u) (u).complex4
#define ffebld_cu_val_complex5(u) (u).complex5
#define ffebld_cu_val_complex6(u) (u).complex6
#define ffebld_cu_val_complex7(u) (u).complex7
#define ffebld_cu_val_complex8(u) (u).complex8
#define ffebld_cu_val_complexdefault ffebld_cu_val_complex1
#define ffebld_cu_val_complexdouble ffebld_cu_val_complex2
#define ffebld_cu_val_complexquad ffebld_cu_val_complex3
#define ffebld_cu_val_character1(u) (u).character1
#define ffebld_cu_val_character2(u) (u).character2
#define ffebld_cu_val_character3(u) (u).character3
#define ffebld_cu_val_character4(u) (u).character4
#define ffebld_cu_val_character5(u) (u).character5
#define ffebld_cu_val_character6(u) (u).character6
#define ffebld_cu_val_character7(u) (u).character7
#define ffebld_cu_val_character8(u) (u).character8
#define ffebld_end_list(b) (*(b) = NULL)
#define ffebld_head(b) ((b)->u.item.head)
#define ffebld_info(b) ((b)->info)
#define ffebld_init_3()
#define ffebld_init_4()
#define ffebld_init_list(l,b) (*(l) = NULL, *(b) = (l))
#define ffebld_item_hook(b) ((b)->u.item.hook)
#define ffebld_item_set_hook(b,h) ((b)->u.item.hook = (h))
#define ffebld_kind(b) (ffeinfo_kind (ffebld_info ((b))))
#define ffebld_kindtype(b) (ffeinfo_kindtype (ffebld_info ((b))))
#define ffebld_labter(b) ((b)->u.labter)
#define ffebld_labtok(b) ((b)->u.labtok)
#define ffebld_left(b) ((b)->u.nonter.left)
#define ffebld_name_string(n) ((n)->name)
#define ffebld_new()							      \
  ((ffebld) malloc_new_kp(ffebld_pool(), "FFEBLD",sizeof(struct _ffebld_)))
#define ffebld_new_any() ffebld_new_none(FFEBLD_opANY)
#define ffebld_new_conter(c) ffebld_new_conter_with_orig((c),NULL)
#define ffebld_new_star() ffebld_new_none(FFEBLD_opSTAR)
#define ffebld_new_uplus(l) ffebld_new_one(FFEBLD_opUPLUS,(l))
#define ffebld_new_uminus(l) ffebld_new_one(FFEBLD_opUMINUS,(l))
#define ffebld_new_add(l,r) ffebld_new_two(FFEBLD_opADD,(l),(r))
#define ffebld_new_subtract(l,r) ffebld_new_two(FFEBLD_opSUBTRACT,(l),(r))
#define ffebld_new_multiply(l,r) ffebld_new_two(FFEBLD_opMULTIPLY,(l),(r))
#define ffebld_new_divide(l,r) ffebld_new_two(FFEBLD_opDIVIDE,(l),(r))
#define ffebld_new_power(l,r) ffebld_new_two(FFEBLD_opPOWER,(l),(r))
#define ffebld_new_bounds(l,r) ffebld_new_two(FFEBLD_opBOUNDS,(l),(r))
#define ffebld_new_concatenate(l,r) ffebld_new_two(FFEBLD_opCONCATENATE,(l),(r))
#define ffebld_new_not(l) ffebld_new_one(FFEBLD_opNOT,(l))
#define ffebld_new_lt(l,r) ffebld_new_two(FFEBLD_opLT,(l),(r))
#define ffebld_new_le(l,r) ffebld_new_two(FFEBLD_opLE,(l),(r))
#define ffebld_new_eq(l,r) ffebld_new_two(FFEBLD_opEQ,(l),(r))
#define ffebld_new_ne(l,r) ffebld_new_two(FFEBLD_opNE,(l),(r))
#define ffebld_new_gt(l,r) ffebld_new_two(FFEBLD_opGT,(l),(r))
#define ffebld_new_ge(l,r) ffebld_new_two(FFEBLD_opGE,(l),(r))
#define ffebld_new_and(l,r) ffebld_new_two(FFEBLD_opAND,(l),(r))
#define ffebld_new_or(l,r) ffebld_new_two(FFEBLD_opOR,(l),(r))
#define ffebld_new_xor(l,r) ffebld_new_two(FFEBLD_opXOR,(l),(r))
#define ffebld_new_eqv(l,r) ffebld_new_two(FFEBLD_opEQV,(l),(r))
#define ffebld_new_neqv(l,r) ffebld_new_two(FFEBLD_opNEQV,(l),(r))
#define ffebld_new_paren(l) ffebld_new_one(FFEBLD_opPAREN,(l))
#define ffebld_new_repeat(l,r) ffebld_new_two(FFEBLD_opREPEAT,(l),(r))
#define ffebld_new_percent_descr(l) ffebld_new_one(FFEBLD_opPERCENT_DESCR,(l))
#define ffebld_new_percent_loc(l) ffebld_new_one(FFEBLD_opPERCENT_LOC,(l))
#define ffebld_new_percent_ref(l) ffebld_new_one(FFEBLD_opPERCENT_REF,(l))
#define ffebld_new_percent_val(l) ffebld_new_one(FFEBLD_opPERCENT_VAL,(l))
#define ffebld_new_complex(l,r) ffebld_new_two(FFEBLD_opCOMPLEX,(l),(r))
#define ffebld_new_convert(l) ffebld_new_one(FFEBLD_opCONVERT,(l))
#define ffebld_new_funcref(l,r) ffebld_new_two(FFEBLD_opFUNCREF,(l),(r))
#define ffebld_new_subrref(l,r) ffebld_new_two(FFEBLD_opSUBRREF,(l),(r))
#define ffebld_new_arrayref(l,r) ffebld_new_two(FFEBLD_opARRAYREF,(l),(r))
#define ffebld_new_substr(l,r) ffebld_new_two(FFEBLD_opSUBSTR,(l),(r))
#define ffebld_new_impdo(l,r) ffebld_new_two(FFEBLD_opIMPDO,(l),(r))
#define ffebld_nonter_hook(b) ((b)->u.nonter.hook)
#define ffebld_nonter_set_hook(b,h) ((b)->u.nonter.hook = (h))
#define ffebld_op(b) ((b)->op)
#define ffebld_pool() (ffebld_pool_stack_.pool)
#define ffebld_rank(b) (ffeinfo_rank (ffebld_info ((b))))
#define ffebld_right(b) ((b)->u.nonter.right)
#define ffebld_set_accter(b,a) ((b)->u.accter.array = (a))
#define ffebld_set_arrter(b,a) ((b)->u.arrter.array = (a))
#define ffebld_set_conter(b,c) ((b)->u.conter.expr = (c))
#define ffebld_set_info(b,i) ((b)->info = (i))
#define ffebld_set_labter(b,l) ((b)->u.labter = (l))
#define ffebld_set_op(b,o) ((b)->op = (o))
#define ffebld_set_head(b,h) ((b)->u.item.head = (h))
#define ffebld_set_left(b,l) ((b)->u.nonter.left = (l))
#define ffebld_set_right(b,r) ((b)->u.nonter.right = (r))
#define ffebld_set_trail(b,t) ((b)->u.item.trail = (t))
#define ffebld_size(b) (ffeinfo_size (ffebld_info ((b))))
#define ffebld_size_known(b) ffebld_size((b))
#define ffebld_symter(b) ((b)->u.symter.symbol)
#define ffebld_symter_generic(b) ((b)->u.symter.generic)
#define ffebld_symter_doiter(b) ((b)->u.symter.do_iter)
#define ffebld_symter_implementation(b) ((b)->u.symter.implementation)
#define ffebld_symter_specific(b) ((b)->u.symter.specific)
#define ffebld_symter_set_generic(b,g) ((b)->u.symter.generic = (g))
#define ffebld_symter_set_implementation(b,i) \
  ((b)->u.symter.implementation = (i))
#define ffebld_symter_set_is_doiter(b,f) ((b)->u.symter.do_iter = (f))
#define ffebld_symter_set_specific(b,s) ((b)->u.symter.specific = (s))
#define ffebld_terminate_0()
#define ffebld_terminate_1()
#define ffebld_terminate_2()
#define ffebld_terminate_3()
#define ffebld_terminate_4()
#define ffebld_trail(b) ((b)->u.item.trail)
#define ffebld_where(b) (ffeinfo_where (ffebld_info ((b))))

/* End of #include file. */

#endif /* ! GCC_F_BLD_H */
