/* bld.c -- Implementation File (module.c template V1.0)
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

   Related Modules:
      None

   Description:
      The primary "output" of the FFE includes ffebld objects, which
      connect expressions, operators, and operands together, along with
      connecting lists of expressions together for argument or dimension
      lists.

   Modifications:
      30-Aug-92	 JCB  1.1
	 Change names of some things for consistency.
*/

/* Include files. */

#include "proj.h"
#include "bld.h"
#include "bit.h"
#include "info.h"
#include "lex.h"
#include "malloc.h"
#include "target.h"
#include "where.h"

/* Externals defined here.  */

const ffebldArity ffebld_arity_op_[(int) FFEBLD_op]
=
{
#define FFEBLD_OP(KWD,NAME,ARITY) ARITY,
#include "bld-op.def"
#undef FFEBLD_OP
};
struct _ffebld_pool_stack_ ffebld_pool_stack_;

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module.	 */

#if FFEBLD_BLANK_
static struct _ffebld_ ffebld_blank_
=
{
  0,
  {FFEINFO_basictypeNONE, FFEINFO_kindtypeNONE, 0, FFEINFO_kindNONE,
   FFEINFO_whereNONE, FFETARGET_charactersizeNONE},
  {NULL, NULL}
};
#endif
#if FFETARGET_okCHARACTER1
static ffebldConstant ffebld_constant_character1_;
#endif
#if FFETARGET_okCHARACTER2
static ffebldConstant ffebld_constant_character2_;
#endif
#if FFETARGET_okCHARACTER3
static ffebldConstant ffebld_constant_character3_;
#endif
#if FFETARGET_okCHARACTER4
static ffebldConstant ffebld_constant_character4_;
#endif
#if FFETARGET_okCHARACTER5
static ffebldConstant ffebld_constant_character5_;
#endif
#if FFETARGET_okCHARACTER6
static ffebldConstant ffebld_constant_character6_;
#endif
#if FFETARGET_okCHARACTER7
static ffebldConstant ffebld_constant_character7_;
#endif
#if FFETARGET_okCHARACTER8
static ffebldConstant ffebld_constant_character8_;
#endif
#if FFETARGET_okCOMPLEX1
static ffebldConstant ffebld_constant_complex1_;
#endif
#if FFETARGET_okCOMPLEX2
static ffebldConstant ffebld_constant_complex2_;
#endif
#if FFETARGET_okCOMPLEX3
static ffebldConstant ffebld_constant_complex3_;
#endif
#if FFETARGET_okCOMPLEX4
static ffebldConstant ffebld_constant_complex4_;
#endif
#if FFETARGET_okCOMPLEX5
static ffebldConstant ffebld_constant_complex5_;
#endif
#if FFETARGET_okCOMPLEX6
static ffebldConstant ffebld_constant_complex6_;
#endif
#if FFETARGET_okCOMPLEX7
static ffebldConstant ffebld_constant_complex7_;
#endif
#if FFETARGET_okCOMPLEX8
static ffebldConstant ffebld_constant_complex8_;
#endif
#if FFETARGET_okINTEGER1
static ffebldConstant ffebld_constant_integer1_;
#endif
#if FFETARGET_okINTEGER2
static ffebldConstant ffebld_constant_integer2_;
#endif
#if FFETARGET_okINTEGER3
static ffebldConstant ffebld_constant_integer3_;
#endif
#if FFETARGET_okINTEGER4
static ffebldConstant ffebld_constant_integer4_;
#endif
#if FFETARGET_okINTEGER5
static ffebldConstant ffebld_constant_integer5_;
#endif
#if FFETARGET_okINTEGER6
static ffebldConstant ffebld_constant_integer6_;
#endif
#if FFETARGET_okINTEGER7
static ffebldConstant ffebld_constant_integer7_;
#endif
#if FFETARGET_okINTEGER8
static ffebldConstant ffebld_constant_integer8_;
#endif
#if FFETARGET_okLOGICAL1
static ffebldConstant ffebld_constant_logical1_;
#endif
#if FFETARGET_okLOGICAL2
static ffebldConstant ffebld_constant_logical2_;
#endif
#if FFETARGET_okLOGICAL3
static ffebldConstant ffebld_constant_logical3_;
#endif
#if FFETARGET_okLOGICAL4
static ffebldConstant ffebld_constant_logical4_;
#endif
#if FFETARGET_okLOGICAL5
static ffebldConstant ffebld_constant_logical5_;
#endif
#if FFETARGET_okLOGICAL6
static ffebldConstant ffebld_constant_logical6_;
#endif
#if FFETARGET_okLOGICAL7
static ffebldConstant ffebld_constant_logical7_;
#endif
#if FFETARGET_okLOGICAL8
static ffebldConstant ffebld_constant_logical8_;
#endif
#if FFETARGET_okREAL1
static ffebldConstant ffebld_constant_real1_;
#endif
#if FFETARGET_okREAL2
static ffebldConstant ffebld_constant_real2_;
#endif
#if FFETARGET_okREAL3
static ffebldConstant ffebld_constant_real3_;
#endif
#if FFETARGET_okREAL4
static ffebldConstant ffebld_constant_real4_;
#endif
#if FFETARGET_okREAL5
static ffebldConstant ffebld_constant_real5_;
#endif
#if FFETARGET_okREAL6
static ffebldConstant ffebld_constant_real6_;
#endif
#if FFETARGET_okREAL7
static ffebldConstant ffebld_constant_real7_;
#endif
#if FFETARGET_okREAL8
static ffebldConstant ffebld_constant_real8_;
#endif
static ffebldConstant ffebld_constant_hollerith_;
static ffebldConstant ffebld_constant_typeless_[FFEBLD_constTYPELESS_LAST
					  - FFEBLD_constTYPELESS_FIRST + 1];

static const char *const ffebld_op_string_[]
=
{
#define FFEBLD_OP(KWD,NAME,ARITY) NAME,
#include "bld-op.def"
#undef FFEBLD_OP
};

/* Static functions (internal). */


/* Internal macros. */

#define integerdefault_ CATX(integer,FFETARGET_ktINTEGERDEFAULT)
#define logicaldefault_ CATX(logical,FFETARGET_ktLOGICALDEFAULT)
#define realdefault_ CATX(real,FFETARGET_ktREALDEFAULT)
#define realdouble_ CATX(real,FFETARGET_ktREALDOUBLE)
#define realquad_ CATX(real,FFETARGET_ktREALQUAD)

/* ffebld_constant_cmp -- Compare two constants a la strcmp

   ffebldConstant c1, c2;
   if (ffebld_constant_cmp(c1,c2) == 0)
       // they're equal, else they're not.

   Returns -1 if c1 < c2, 0 if c1 == c2, +1 if c1 == c2.  */

int
ffebld_constant_cmp (ffebldConstant c1, ffebldConstant c2)
{
  if (c1 == c2)
    return 0;

  assert (ffebld_constant_type (c1) == ffebld_constant_type (c2));

  switch (ffebld_constant_type (c1))
    {
#if FFETARGET_okINTEGER1
    case FFEBLD_constINTEGER1:
      return ffetarget_cmp_integer1 (ffebld_constant_integer1 (c1),
				     ffebld_constant_integer1 (c2));
#endif

#if FFETARGET_okINTEGER2
    case FFEBLD_constINTEGER2:
      return ffetarget_cmp_integer2 (ffebld_constant_integer2 (c1),
				     ffebld_constant_integer2 (c2));
#endif

#if FFETARGET_okINTEGER3
    case FFEBLD_constINTEGER3:
      return ffetarget_cmp_integer3 (ffebld_constant_integer3 (c1),
				     ffebld_constant_integer3 (c2));
#endif

#if FFETARGET_okINTEGER4
    case FFEBLD_constINTEGER4:
      return ffetarget_cmp_integer4 (ffebld_constant_integer4 (c1),
				     ffebld_constant_integer4 (c2));
#endif

#if FFETARGET_okINTEGER5
    case FFEBLD_constINTEGER5:
      return ffetarget_cmp_integer5 (ffebld_constant_integer5 (c1),
				     ffebld_constant_integer5 (c2));
#endif

#if FFETARGET_okINTEGER6
    case FFEBLD_constINTEGER6:
      return ffetarget_cmp_integer6 (ffebld_constant_integer6 (c1),
				     ffebld_constant_integer6 (c2));
#endif

#if FFETARGET_okINTEGER7
    case FFEBLD_constINTEGER7:
      return ffetarget_cmp_integer7 (ffebld_constant_integer7 (c1),
				     ffebld_constant_integer7 (c2));
#endif

#if FFETARGET_okINTEGER8
    case FFEBLD_constINTEGER8:
      return ffetarget_cmp_integer8 (ffebld_constant_integer8 (c1),
				     ffebld_constant_integer8 (c2));
#endif

#if FFETARGET_okLOGICAL1
    case FFEBLD_constLOGICAL1:
      return ffetarget_cmp_logical1 (ffebld_constant_logical1 (c1),
				     ffebld_constant_logical1 (c2));
#endif

#if FFETARGET_okLOGICAL2
    case FFEBLD_constLOGICAL2:
      return ffetarget_cmp_logical2 (ffebld_constant_logical2 (c1),
				     ffebld_constant_logical2 (c2));
#endif

#if FFETARGET_okLOGICAL3
    case FFEBLD_constLOGICAL3:
      return ffetarget_cmp_logical3 (ffebld_constant_logical3 (c1),
				     ffebld_constant_logical3 (c2));
#endif

#if FFETARGET_okLOGICAL4
    case FFEBLD_constLOGICAL4:
      return ffetarget_cmp_logical4 (ffebld_constant_logical4 (c1),
				     ffebld_constant_logical4 (c2));
#endif

#if FFETARGET_okLOGICAL5
    case FFEBLD_constLOGICAL5:
      return ffetarget_cmp_logical5 (ffebld_constant_logical5 (c1),
				     ffebld_constant_logical5 (c2));
#endif

#if FFETARGET_okLOGICAL6
    case FFEBLD_constLOGICAL6:
      return ffetarget_cmp_logical6 (ffebld_constant_logical6 (c1),
				     ffebld_constant_logical6 (c2));
#endif

#if FFETARGET_okLOGICAL7
    case FFEBLD_constLOGICAL7:
      return ffetarget_cmp_logical7 (ffebld_constant_logical7 (c1),
				     ffebld_constant_logical7 (c2));
#endif

#if FFETARGET_okLOGICAL8
    case FFEBLD_constLOGICAL8:
      return ffetarget_cmp_logical8 (ffebld_constant_logical8 (c1),
				     ffebld_constant_logical8 (c2));
#endif

#if FFETARGET_okREAL1
    case FFEBLD_constREAL1:
      return ffetarget_cmp_real1 (ffebld_constant_real1 (c1),
				  ffebld_constant_real1 (c2));
#endif

#if FFETARGET_okREAL2
    case FFEBLD_constREAL2:
      return ffetarget_cmp_real2 (ffebld_constant_real2 (c1),
				  ffebld_constant_real2 (c2));
#endif

#if FFETARGET_okREAL3
    case FFEBLD_constREAL3:
      return ffetarget_cmp_real3 (ffebld_constant_real3 (c1),
				  ffebld_constant_real3 (c2));
#endif

#if FFETARGET_okREAL4
    case FFEBLD_constREAL4:
      return ffetarget_cmp_real4 (ffebld_constant_real4 (c1),
				  ffebld_constant_real4 (c2));
#endif

#if FFETARGET_okREAL5
    case FFEBLD_constREAL5:
      return ffetarget_cmp_real5 (ffebld_constant_real5 (c1),
				  ffebld_constant_real5 (c2));
#endif

#if FFETARGET_okREAL6
    case FFEBLD_constREAL6:
      return ffetarget_cmp_real6 (ffebld_constant_real6 (c1),
				  ffebld_constant_real6 (c2));
#endif

#if FFETARGET_okREAL7
    case FFEBLD_constREAL7:
      return ffetarget_cmp_real7 (ffebld_constant_real7 (c1),
				  ffebld_constant_real7 (c2));
#endif

#if FFETARGET_okREAL8
    case FFEBLD_constREAL8:
      return ffetarget_cmp_real8 (ffebld_constant_real8 (c1),
				  ffebld_constant_real8 (c2));
#endif

#if FFETARGET_okCHARACTER1
    case FFEBLD_constCHARACTER1:
      return ffetarget_cmp_character1 (ffebld_constant_character1 (c1),
				       ffebld_constant_character1 (c2));
#endif

#if FFETARGET_okCHARACTER2
    case FFEBLD_constCHARACTER2:
      return ffetarget_cmp_character2 (ffebld_constant_character2 (c1),
				       ffebld_constant_character2 (c2));
#endif

#if FFETARGET_okCHARACTER3
    case FFEBLD_constCHARACTER3:
      return ffetarget_cmp_character3 (ffebld_constant_character3 (c1),
				       ffebld_constant_character3 (c2));
#endif

#if FFETARGET_okCHARACTER4
    case FFEBLD_constCHARACTER4:
      return ffetarget_cmp_character4 (ffebld_constant_character4 (c1),
				       ffebld_constant_character4 (c2));
#endif

#if FFETARGET_okCHARACTER5
    case FFEBLD_constCHARACTER5:
      return ffetarget_cmp_character5 (ffebld_constant_character5 (c1),
				       ffebld_constant_character5 (c2));
#endif

#if FFETARGET_okCHARACTER6
    case FFEBLD_constCHARACTER6:
      return ffetarget_cmp_character6 (ffebld_constant_character6 (c1),
				       ffebld_constant_character6 (c2));
#endif

#if FFETARGET_okCHARACTER7
    case FFEBLD_constCHARACTER7:
      return ffetarget_cmp_character7 (ffebld_constant_character7 (c1),
				       ffebld_constant_character7 (c2));
#endif

#if FFETARGET_okCHARACTER8
    case FFEBLD_constCHARACTER8:
      return ffetarget_cmp_character8 (ffebld_constant_character8 (c1),
				       ffebld_constant_character8 (c2));
#endif

    default:
      assert ("bad constant type" == NULL);
      return 0;
    }
}

/* ffebld_constant_is_magical -- Determine if integer is "magical"

   ffebldConstant c;
   if (ffebld_constant_is_magical(c))
       // it is 2**(n-1), where n is # bits in ffetargetIntegerDefault type
       // (this test is important for 2's-complement machines only).  */

bool
ffebld_constant_is_magical (ffebldConstant c)
{
  switch (ffebld_constant_type (c))
    {
    case FFEBLD_constINTEGERDEFAULT:
      return ffetarget_integerdefault_is_magical (ffebld_constant_integer1 (c));

    default:
      return FALSE;
    }
}

/* Determine if constant is zero.  Used to ensure step count
   for DO loops isn't zero, also to determine if values will
   be binary zeros, so not entirely portable at this point.  */

bool
ffebld_constant_is_zero (ffebldConstant c)
{
  switch (ffebld_constant_type (c))
    {
#if FFETARGET_okINTEGER1
    case FFEBLD_constINTEGER1:
      return ffebld_constant_integer1 (c) == 0;
#endif

#if FFETARGET_okINTEGER2
    case FFEBLD_constINTEGER2:
      return ffebld_constant_integer2 (c) == 0;
#endif

#if FFETARGET_okINTEGER3
    case FFEBLD_constINTEGER3:
      return ffebld_constant_integer3 (c) == 0;
#endif

#if FFETARGET_okINTEGER4
    case FFEBLD_constINTEGER4:
      return ffebld_constant_integer4 (c) == 0;
#endif

#if FFETARGET_okINTEGER5
    case FFEBLD_constINTEGER5:
      return ffebld_constant_integer5 (c) == 0;
#endif

#if FFETARGET_okINTEGER6
    case FFEBLD_constINTEGER6:
      return ffebld_constant_integer6 (c) == 0;
#endif

#if FFETARGET_okINTEGER7
    case FFEBLD_constINTEGER7:
      return ffebld_constant_integer7 (c) == 0;
#endif

#if FFETARGET_okINTEGER8
    case FFEBLD_constINTEGER8:
      return ffebld_constant_integer8 (c) == 0;
#endif

#if FFETARGET_okLOGICAL1
    case FFEBLD_constLOGICAL1:
      return ffebld_constant_logical1 (c) == 0;
#endif

#if FFETARGET_okLOGICAL2
    case FFEBLD_constLOGICAL2:
      return ffebld_constant_logical2 (c) == 0;
#endif

#if FFETARGET_okLOGICAL3
    case FFEBLD_constLOGICAL3:
      return ffebld_constant_logical3 (c) == 0;
#endif

#if FFETARGET_okLOGICAL4
    case FFEBLD_constLOGICAL4:
      return ffebld_constant_logical4 (c) == 0;
#endif

#if FFETARGET_okLOGICAL5
    case FFEBLD_constLOGICAL5:
      return ffebld_constant_logical5 (c) == 0;
#endif

#if FFETARGET_okLOGICAL6
    case FFEBLD_constLOGICAL6:
      return ffebld_constant_logical6 (c) == 0;
#endif

#if FFETARGET_okLOGICAL7
    case FFEBLD_constLOGICAL7:
      return ffebld_constant_logical7 (c) == 0;
#endif

#if FFETARGET_okLOGICAL8
    case FFEBLD_constLOGICAL8:
      return ffebld_constant_logical8 (c) == 0;
#endif

#if FFETARGET_okREAL1
    case FFEBLD_constREAL1:
      return ffetarget_iszero_real1 (ffebld_constant_real1 (c));
#endif

#if FFETARGET_okREAL2
    case FFEBLD_constREAL2:
      return ffetarget_iszero_real2 (ffebld_constant_real2 (c));
#endif

#if FFETARGET_okREAL3
    case FFEBLD_constREAL3:
      return ffetarget_iszero_real3 (ffebld_constant_real3 (c));
#endif

#if FFETARGET_okREAL4
    case FFEBLD_constREAL4:
      return ffetarget_iszero_real4 (ffebld_constant_real4 (c));
#endif

#if FFETARGET_okREAL5
    case FFEBLD_constREAL5:
      return ffetarget_iszero_real5 (ffebld_constant_real5 (c));
#endif

#if FFETARGET_okREAL6
    case FFEBLD_constREAL6:
      return ffetarget_iszero_real6 (ffebld_constant_real6 (c));
#endif

#if FFETARGET_okREAL7
    case FFEBLD_constREAL7:
      return ffetarget_iszero_real7 (ffebld_constant_real7 (c));
#endif

#if FFETARGET_okREAL8
    case FFEBLD_constREAL8:
      return ffetarget_iszero_real8 (ffebld_constant_real8 (c));
#endif

#if FFETARGET_okCOMPLEX1
    case FFEBLD_constCOMPLEX1:
      return ffetarget_iszero_real1 (ffebld_constant_complex1 (c).real)
     && ffetarget_iszero_real1 (ffebld_constant_complex1 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX2
    case FFEBLD_constCOMPLEX2:
      return ffetarget_iszero_real2 (ffebld_constant_complex2 (c).real)
     && ffetarget_iszero_real2 (ffebld_constant_complex2 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX3
    case FFEBLD_constCOMPLEX3:
      return ffetarget_iszero_real3 (ffebld_constant_complex3 (c).real)
     && ffetarget_iszero_real3 (ffebld_constant_complex3 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX4
    case FFEBLD_constCOMPLEX4:
      return ffetarget_iszero_real4 (ffebld_constant_complex4 (c).real)
     && ffetarget_iszero_real4 (ffebld_constant_complex4 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX5
    case FFEBLD_constCOMPLEX5:
      return ffetarget_iszero_real5 (ffebld_constant_complex5 (c).real)
     && ffetarget_iszero_real5 (ffebld_constant_complex5 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX6
    case FFEBLD_constCOMPLEX6:
      return ffetarget_iszero_real6 (ffebld_constant_complex6 (c).real)
     && ffetarget_iszero_real6 (ffebld_constant_complex6 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX7
    case FFEBLD_constCOMPLEX7:
      return ffetarget_iszero_real7 (ffebld_constant_complex7 (c).real)
     && ffetarget_iszero_real7 (ffebld_constant_complex7 (c).imaginary);
#endif

#if FFETARGET_okCOMPLEX8
    case FFEBLD_constCOMPLEX8:
      return ffetarget_iszero_real8 (ffebld_constant_complex8 (c).real)
     && ffetarget_iszero_real8 (ffebld_constant_complex8 (c).imaginary);
#endif

#if FFETARGET_okCHARACTER1
    case FFEBLD_constCHARACTER1:
      return ffetarget_iszero_character1 (ffebld_constant_character1 (c));
#endif

#if FFETARGET_okCHARACTER2 || FFETARGET_okCHARACTER3  /* ... */
#error "no support for these!!"
#endif

    case FFEBLD_constHOLLERITH:
      return ffetarget_iszero_hollerith (ffebld_constant_hollerith (c));

    case FFEBLD_constBINARY_MIL:
    case FFEBLD_constBINARY_VXT:
    case FFEBLD_constOCTAL_MIL:
    case FFEBLD_constOCTAL_VXT:
    case FFEBLD_constHEX_X_MIL:
    case FFEBLD_constHEX_X_VXT:
    case FFEBLD_constHEX_Z_MIL:
    case FFEBLD_constHEX_Z_VXT:
      return ffetarget_iszero_typeless (ffebld_constant_typeless (c));

    default:
      return FALSE;
    }
}

/* ffebld_constant_new_character1 -- Return character1 constant object from token

   See prototype.  */

#if FFETARGET_okCHARACTER1
ffebldConstant
ffebld_constant_new_character1 (ffelexToken t)
{
  ffetargetCharacter1 val;

  ffetarget_character1 (&val, t, ffebld_constant_pool());
  return ffebld_constant_new_character1_val (val);
}

#endif
/* ffebld_constant_new_character1_val -- Return an character1 constant object

   See prototype.  */

#if FFETARGET_okCHARACTER1
ffebldConstant
ffebld_constant_new_character1_val (ffetargetCharacter1 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  ffetarget_verify_character1 (ffebld_constant_pool(), val);

  for (c = (ffebldConstant) &ffebld_constant_character1_;
       c->next != NULL;
       c = c->next)
    {
      malloc_verify_kp (ffebld_constant_pool(),
			c->next,
			sizeof (*(c->next)));
      ffetarget_verify_character1 (ffebld_constant_pool(),
				   ffebld_constant_character1 (c->next));
      cmp = ffetarget_cmp_character1 (val,
				      ffebld_constant_character1 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constCHARACTER1",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constCHARACTER1;
  nc->u.character1 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_complex1 -- Return complex1 constant object from token

   See prototype.  */

#if FFETARGET_okCOMPLEX1
ffebldConstant
ffebld_constant_new_complex1 (ffebldConstant real,
			      ffebldConstant imaginary)
{
  ffetargetComplex1 val;

  val.real = ffebld_constant_real1 (real);
  val.imaginary = ffebld_constant_real1 (imaginary);
  return ffebld_constant_new_complex1_val (val);
}

#endif
/* ffebld_constant_new_complex1_val -- Return a complex1 constant object

   See prototype.  */

#if FFETARGET_okCOMPLEX1
ffebldConstant
ffebld_constant_new_complex1_val (ffetargetComplex1 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_complex1_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_real1 (val.real, ffebld_constant_complex1 (c->next).real);
      if (cmp == 0)
	cmp = ffetarget_cmp_real1 (val.imaginary,
			      ffebld_constant_complex1 (c->next).imaginary);
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constCOMPLEX1",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constCOMPLEX1;
  nc->u.complex1 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_complex2 -- Return complex2 constant object from token

   See prototype.  */

#if FFETARGET_okCOMPLEX2
ffebldConstant
ffebld_constant_new_complex2 (ffebldConstant real,
			      ffebldConstant imaginary)
{
  ffetargetComplex2 val;

  val.real = ffebld_constant_real2 (real);
  val.imaginary = ffebld_constant_real2 (imaginary);
  return ffebld_constant_new_complex2_val (val);
}

#endif
/* ffebld_constant_new_complex2_val -- Return a complex2 constant object

   See prototype.  */

#if FFETARGET_okCOMPLEX2
ffebldConstant
ffebld_constant_new_complex2_val (ffetargetComplex2 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_complex2_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_real2 (val.real, ffebld_constant_complex2 (c->next).real);
      if (cmp == 0)
	cmp = ffetarget_cmp_real2 (val.imaginary,
			      ffebld_constant_complex2 (c->next).imaginary);
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constCOMPLEX2",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constCOMPLEX2;
  nc->u.complex2 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_hollerith -- Return hollerith constant object from token

   See prototype.  */

ffebldConstant
ffebld_constant_new_hollerith (ffelexToken t)
{
  ffetargetHollerith val;

  ffetarget_hollerith (&val, t, ffebld_constant_pool());
  return ffebld_constant_new_hollerith_val (val);
}

/* ffebld_constant_new_hollerith_val -- Return an hollerith constant object

   See prototype.  */

ffebldConstant
ffebld_constant_new_hollerith_val (ffetargetHollerith val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_hollerith_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_hollerith (val, ffebld_constant_hollerith (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constHOLLERITH",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constHOLLERITH;
  nc->u.hollerith = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

/* ffebld_constant_new_integer1 -- Return integer1 constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

#if FFETARGET_okINTEGER1
ffebldConstant
ffebld_constant_new_integer1 (ffelexToken t)
{
  ffetargetInteger1 val;

  assert (ffelex_token_type (t) == FFELEX_typeNUMBER);

  ffetarget_integer1 (&val, t);
  return ffebld_constant_new_integer1_val (val);
}

#endif
/* ffebld_constant_new_integer1_val -- Return an integer1 constant object

   See prototype.  */

#if FFETARGET_okINTEGER1
ffebldConstant
ffebld_constant_new_integer1_val (ffetargetInteger1 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_integer1_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_integer1 (val, ffebld_constant_integer1 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constINTEGER1",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constINTEGER1;
  nc->u.integer1 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_integer2_val -- Return an integer2 constant object

   See prototype.  */

#if FFETARGET_okINTEGER2
ffebldConstant
ffebld_constant_new_integer2_val (ffetargetInteger2 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_integer2_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_integer2 (val, ffebld_constant_integer2 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constINTEGER2",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constINTEGER2;
  nc->u.integer2 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_integer3_val -- Return an integer3 constant object

   See prototype.  */

#if FFETARGET_okINTEGER3
ffebldConstant
ffebld_constant_new_integer3_val (ffetargetInteger3 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_integer3_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_integer3 (val, ffebld_constant_integer3 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constINTEGER3",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constINTEGER3;
  nc->u.integer3 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_integer4_val -- Return an integer4 constant object

   See prototype.  */

#if FFETARGET_okINTEGER4
ffebldConstant
ffebld_constant_new_integer4_val (ffetargetInteger4 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_integer4_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_integer4 (val, ffebld_constant_integer4 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constINTEGER4",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constINTEGER4;
  nc->u.integer4 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_integerbinary -- Return binary constant object from token

   See prototype.

   Parses the token as a binary integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_integerbinary (ffelexToken t)
{
  ffetargetIntegerDefault val;

  assert ((ffelex_token_type (t) == FFELEX_typeNAME)
	  || (ffelex_token_type (t) == FFELEX_typeNUMBER));

  ffetarget_integerbinary (&val, t);
  return ffebld_constant_new_integerdefault_val (val);
}

/* ffebld_constant_new_integerhex -- Return hex constant object from token

   See prototype.

   Parses the token as a hex integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_integerhex (ffelexToken t)
{
  ffetargetIntegerDefault val;

  assert ((ffelex_token_type (t) == FFELEX_typeNAME)
	  || (ffelex_token_type (t) == FFELEX_typeNUMBER));

  ffetarget_integerhex (&val, t);
  return ffebld_constant_new_integerdefault_val (val);
}

/* ffebld_constant_new_integeroctal -- Return octal constant object from token

   See prototype.

   Parses the token as a octal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_integeroctal (ffelexToken t)
{
  ffetargetIntegerDefault val;

  assert ((ffelex_token_type (t) == FFELEX_typeNAME)
	  || (ffelex_token_type (t) == FFELEX_typeNUMBER));

  ffetarget_integeroctal (&val, t);
  return ffebld_constant_new_integerdefault_val (val);
}

/* ffebld_constant_new_logical1 -- Return logical1 constant object from token

   See prototype.

   Parses the token as a decimal logical constant, thus it must be an
   FFELEX_typeNUMBER.  */

#if FFETARGET_okLOGICAL1
ffebldConstant
ffebld_constant_new_logical1 (bool truth)
{
  ffetargetLogical1 val;

  ffetarget_logical1 (&val, truth);
  return ffebld_constant_new_logical1_val (val);
}

#endif
/* ffebld_constant_new_logical1_val -- Return a logical1 constant object

   See prototype.  */

#if FFETARGET_okLOGICAL1
ffebldConstant
ffebld_constant_new_logical1_val (ffetargetLogical1 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_logical1_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_logical1 (val, ffebld_constant_logical1 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constLOGICAL1",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constLOGICAL1;
  nc->u.logical1 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_logical2_val -- Return a logical2 constant object

   See prototype.  */

#if FFETARGET_okLOGICAL2
ffebldConstant
ffebld_constant_new_logical2_val (ffetargetLogical2 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_logical2_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_logical2 (val, ffebld_constant_logical2 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constLOGICAL2",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constLOGICAL2;
  nc->u.logical2 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_logical3_val -- Return a logical3 constant object

   See prototype.  */

#if FFETARGET_okLOGICAL3
ffebldConstant
ffebld_constant_new_logical3_val (ffetargetLogical3 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_logical3_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_logical3 (val, ffebld_constant_logical3 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constLOGICAL3",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constLOGICAL3;
  nc->u.logical3 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_logical4_val -- Return a logical4 constant object

   See prototype.  */

#if FFETARGET_okLOGICAL4
ffebldConstant
ffebld_constant_new_logical4_val (ffetargetLogical4 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_logical4_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_logical4 (val, ffebld_constant_logical4 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constLOGICAL4",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constLOGICAL4;
  nc->u.logical4 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_real1 -- Return real1 constant object from token

   See prototype.  */

#if FFETARGET_okREAL1
ffebldConstant
ffebld_constant_new_real1 (ffelexToken integer, ffelexToken decimal,
      ffelexToken fraction, ffelexToken exponent, ffelexToken exponent_sign,
			   ffelexToken exponent_digits)
{
  ffetargetReal1 val;

  ffetarget_real1 (&val,
      integer, decimal, fraction, exponent, exponent_sign, exponent_digits);
  return ffebld_constant_new_real1_val (val);
}

#endif
/* ffebld_constant_new_real1_val -- Return an real1 constant object

   See prototype.  */

#if FFETARGET_okREAL1
ffebldConstant
ffebld_constant_new_real1_val (ffetargetReal1 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_real1_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_real1 (val, ffebld_constant_real1 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constREAL1",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constREAL1;
  nc->u.real1 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_real2 -- Return real2 constant object from token

   See prototype.  */

#if FFETARGET_okREAL2
ffebldConstant
ffebld_constant_new_real2 (ffelexToken integer, ffelexToken decimal,
      ffelexToken fraction, ffelexToken exponent, ffelexToken exponent_sign,
			   ffelexToken exponent_digits)
{
  ffetargetReal2 val;

  ffetarget_real2 (&val,
      integer, decimal, fraction, exponent, exponent_sign, exponent_digits);
  return ffebld_constant_new_real2_val (val);
}

#endif
/* ffebld_constant_new_real2_val -- Return an real2 constant object

   See prototype.  */

#if FFETARGET_okREAL2
ffebldConstant
ffebld_constant_new_real2_val (ffetargetReal2 val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_real2_;
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_real2 (val, ffebld_constant_real2 (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constREAL2",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = FFEBLD_constREAL2;
  nc->u.real2 = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

#endif
/* ffebld_constant_new_typeless_bm -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_bm (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_binarymil (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constBINARY_MIL, val);
}

/* ffebld_constant_new_typeless_bv -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_bv (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_binaryvxt (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constBINARY_VXT, val);
}

/* ffebld_constant_new_typeless_hxm -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_hxm (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_hexxmil (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constHEX_X_MIL, val);
}

/* ffebld_constant_new_typeless_hxv -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_hxv (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_hexxvxt (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constHEX_X_VXT, val);
}

/* ffebld_constant_new_typeless_hzm -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_hzm (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_hexzmil (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constHEX_Z_MIL, val);
}

/* ffebld_constant_new_typeless_hzv -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_hzv (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_hexzvxt (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constHEX_Z_VXT, val);
}

/* ffebld_constant_new_typeless_om -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_om (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_octalmil (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constOCTAL_MIL, val);
}

/* ffebld_constant_new_typeless_ov -- Return typeless constant object from token

   See prototype.

   Parses the token as a decimal integer constant, thus it must be an
   FFELEX_typeNUMBER.  */

ffebldConstant
ffebld_constant_new_typeless_ov (ffelexToken t)
{
  ffetargetTypeless val;

  ffetarget_octalvxt (&val, t);
  return ffebld_constant_new_typeless_val (FFEBLD_constOCTAL_VXT, val);
}

/* ffebld_constant_new_typeless_val -- Return a typeless constant object

   See prototype.  */

ffebldConstant
ffebld_constant_new_typeless_val (ffebldConst type, ffetargetTypeless val)
{
  ffebldConstant c;
  ffebldConstant nc;
  int cmp;

  for (c = (ffebldConstant) &ffebld_constant_typeless_[type
					      - FFEBLD_constTYPELESS_FIRST];
       c->next != NULL;
       c = c->next)
    {
      cmp = ffetarget_cmp_typeless (val, ffebld_constant_typeless (c->next));
      if (cmp == 0)
	return c->next;
      if (cmp > 0)
	break;
    }

  nc = malloc_new_kp (ffebld_constant_pool(),
		      "FFEBLD_constTYPELESS",
		      sizeof (*nc));
  nc->next = c->next;
  nc->consttype = type;
  nc->u.typeless = val;
#ifdef FFECOM_constantHOOK
  nc->hook = FFECOM_constantNULL;
#endif
  c->next = nc;

  return nc;
}

/* ffebld_constantarray_get -- Get a value from an array of constants

   See prototype.  */

ffebldConstantUnion
ffebld_constantarray_get (ffebldConstantArray array, ffeinfoBasictype bt,
			  ffeinfoKindtype kt, ffetargetOffset offset)
{
  ffebldConstantUnion u;

  switch (bt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (kt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  u.integer1 = *(array.integer1 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  u.integer2 = *(array.integer2 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  u.integer3 = *(array.integer3 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  u.integer4 = *(array.integer4 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  u.integer5 = *(array.integer5 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  u.integer6 = *(array.integer6 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  u.integer7 = *(array.integer7 + offset);
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  u.integer8 = *(array.integer8 + offset);
	  break;
#endif

	default:
	  assert ("bad INTEGER kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  u.logical1 = *(array.logical1 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  u.logical2 = *(array.logical2 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  u.logical3 = *(array.logical3 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  u.logical4 = *(array.logical4 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  u.logical5 = *(array.logical5 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  u.logical6 = *(array.logical6 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  u.logical7 = *(array.logical7 + offset);
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  u.logical8 = *(array.logical8 + offset);
	  break;
#endif

	default:
	  assert ("bad LOGICAL kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  u.real1 = *(array.real1 + offset);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  u.real2 = *(array.real2 + offset);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  u.real3 = *(array.real3 + offset);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  u.real4 = *(array.real4 + offset);
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  u.real5 = *(array.real5 + offset);
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  u.real6 = *(array.real6 + offset);
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  u.real7 = *(array.real7 + offset);
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  u.real8 = *(array.real8 + offset);
	  break;
#endif

	default:
	  assert ("bad REAL kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  u.complex1 = *(array.complex1 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  u.complex2 = *(array.complex2 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  u.complex3 = *(array.complex3 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  u.complex4 = *(array.complex4 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  u.complex5 = *(array.complex5 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  u.complex6 = *(array.complex6 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  u.complex7 = *(array.complex7 + offset);
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  u.complex8 = *(array.complex8 + offset);
	  break;
#endif

	default:
	  assert ("bad COMPLEX kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (kt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  u.character1.length = 1;
	  u.character1.text = array.character1 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  u.character2.length = 1;
	  u.character2.text = array.character2 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  u.character3.length = 1;
	  u.character3.text = array.character3 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  u.character4.length = 1;
	  u.character4.text = array.character4 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  u.character5.length = 1;
	  u.character5.text = array.character5 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  u.character6.length = 1;
	  u.character6.text = array.character6 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  u.character7.length = 1;
	  u.character7.text = array.character7 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  u.character8.length = 1;
	  u.character8.text = array.character8 + offset;
	  break;
#endif

	default:
	  assert ("bad CHARACTER kindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad basictype" == NULL);
      break;
    }

  return u;
}

/* ffebld_constantarray_new -- Make an array of constants

   See prototype.  */

ffebldConstantArray
ffebld_constantarray_new (ffeinfoBasictype bt,
			  ffeinfoKindtype kt, ffetargetOffset size)
{
  ffebldConstantArray ptr;

  switch (bt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (kt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  ptr.integer1 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger1),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  ptr.integer2 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger2),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  ptr.integer3 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger3),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  ptr.integer4 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger4),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  ptr.integer5 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger5),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  ptr.integer6 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger6),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  ptr.integer7 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger7),
					 0);
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  ptr.integer8 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetInteger8),
					 0);
	  break;
#endif

	default:
	  assert ("bad INTEGER kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  ptr.logical1 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical1),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  ptr.logical2 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical2),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  ptr.logical3 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical3),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  ptr.logical4 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical4),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  ptr.logical5 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical5),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  ptr.logical6 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical6),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  ptr.logical7 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical7),
					 0);
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  ptr.logical8 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetLogical8),
					 0);
	  break;
#endif

	default:
	  assert ("bad LOGICAL kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  ptr.real1 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal1),
				      0);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  ptr.real2 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal2),
				      0);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  ptr.real3 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal3),
				      0);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  ptr.real4 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal4),
				      0);
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  ptr.real5 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal5),
				      0);
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  ptr.real6 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal6),
				      0);
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  ptr.real7 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal7),
				      0);
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  ptr.real8 = malloc_new_zkp (ffebld_constant_pool(),
				      "ffebldConstantArray",
				      size *= sizeof (ffetargetReal8),
				      0);
	  break;
#endif

	default:
	  assert ("bad REAL kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  ptr.complex1 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex1),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  ptr.complex2 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex2),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  ptr.complex3 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex3),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  ptr.complex4 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex4),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  ptr.complex5 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex5),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  ptr.complex6 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex6),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  ptr.complex7 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex7),
					 0);
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  ptr.complex8 = malloc_new_zkp (ffebld_constant_pool(),
					 "ffebldConstantArray",
					 size *= sizeof (ffetargetComplex8),
					 0);
	  break;
#endif

	default:
	  assert ("bad COMPLEX kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (kt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  ptr.character1 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit1),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  ptr.character2 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit2),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  ptr.character3 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit3),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  ptr.character4 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit4),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  ptr.character5 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit5),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  ptr.character6 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit6),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  ptr.character7 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit7),
					   0);
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  ptr.character8 = malloc_new_zkp (ffebld_constant_pool(),
					   "ffebldConstantArray",
					   size
					   *= sizeof (ffetargetCharacterUnit8),
					   0);
	  break;
#endif

	default:
	  assert ("bad CHARACTER kindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad basictype" == NULL);
      break;
    }

  return ptr;
}

/* ffebld_constantarray_preparray -- Prepare for copy between arrays

   See prototype.

   Like _prepare, but the source is an array instead of a single-value
   constant.  */

void
ffebld_constantarray_preparray (void **aptr, void **cptr, size_t *size,
       ffebldConstantArray array, ffeinfoBasictype abt, ffeinfoKindtype akt,
		   ffetargetOffset offset, ffebldConstantArray source_array,
				ffeinfoBasictype cbt, ffeinfoKindtype ckt)
{
  switch (abt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (akt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  *aptr = array.integer1 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  *aptr = array.integer2 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  *aptr = array.integer3 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  *aptr = array.integer4 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  *aptr = array.integer5 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  *aptr = array.integer6 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  *aptr = array.integer7 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  *aptr = array.integer8 + offset;
	  break;
#endif

	default:
	  assert ("bad INTEGER akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (akt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  *aptr = array.logical1 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  *aptr = array.logical2 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  *aptr = array.logical3 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  *aptr = array.logical4 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  *aptr = array.logical5 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  *aptr = array.logical6 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  *aptr = array.logical7 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  *aptr = array.logical8 + offset;
	  break;
#endif

	default:
	  assert ("bad LOGICAL akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (akt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  *aptr = array.real1 + offset;
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  *aptr = array.real2 + offset;
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  *aptr = array.real3 + offset;
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  *aptr = array.real4 + offset;
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  *aptr = array.real5 + offset;
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  *aptr = array.real6 + offset;
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  *aptr = array.real7 + offset;
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  *aptr = array.real8 + offset;
	  break;
#endif

	default:
	  assert ("bad REAL akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (akt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  *aptr = array.complex1 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  *aptr = array.complex2 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  *aptr = array.complex3 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  *aptr = array.complex4 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  *aptr = array.complex5 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  *aptr = array.complex6 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  *aptr = array.complex7 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  *aptr = array.complex8 + offset;
	  break;
#endif

	default:
	  assert ("bad COMPLEX akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (akt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  *aptr = array.character1 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  *aptr = array.character2 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  *aptr = array.character3 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  *aptr = array.character4 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  *aptr = array.character5 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  *aptr = array.character6 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  *aptr = array.character7 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  *aptr = array.character8 + offset;
	  break;
#endif

	default:
	  assert ("bad CHARACTER akindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad abasictype" == NULL);
      break;
    }

  switch (cbt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (ckt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  *cptr = source_array.integer1;
	  *size = sizeof (*source_array.integer1);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  *cptr = source_array.integer2;
	  *size = sizeof (*source_array.integer2);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  *cptr = source_array.integer3;
	  *size = sizeof (*source_array.integer3);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  *cptr = source_array.integer4;
	  *size = sizeof (*source_array.integer4);
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  *cptr = source_array.integer5;
	  *size = sizeof (*source_array.integer5);
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  *cptr = source_array.integer6;
	  *size = sizeof (*source_array.integer6);
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  *cptr = source_array.integer7;
	  *size = sizeof (*source_array.integer7);
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  *cptr = source_array.integer8;
	  *size = sizeof (*source_array.integer8);
	  break;
#endif

	default:
	  assert ("bad INTEGER ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (ckt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  *cptr = source_array.logical1;
	  *size = sizeof (*source_array.logical1);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  *cptr = source_array.logical2;
	  *size = sizeof (*source_array.logical2);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  *cptr = source_array.logical3;
	  *size = sizeof (*source_array.logical3);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  *cptr = source_array.logical4;
	  *size = sizeof (*source_array.logical4);
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  *cptr = source_array.logical5;
	  *size = sizeof (*source_array.logical5);
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  *cptr = source_array.logical6;
	  *size = sizeof (*source_array.logical6);
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  *cptr = source_array.logical7;
	  *size = sizeof (*source_array.logical7);
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  *cptr = source_array.logical8;
	  *size = sizeof (*source_array.logical8);
	  break;
#endif

	default:
	  assert ("bad LOGICAL ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ckt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  *cptr = source_array.real1;
	  *size = sizeof (*source_array.real1);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  *cptr = source_array.real2;
	  *size = sizeof (*source_array.real2);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  *cptr = source_array.real3;
	  *size = sizeof (*source_array.real3);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  *cptr = source_array.real4;
	  *size = sizeof (*source_array.real4);
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  *cptr = source_array.real5;
	  *size = sizeof (*source_array.real5);
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  *cptr = source_array.real6;
	  *size = sizeof (*source_array.real6);
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  *cptr = source_array.real7;
	  *size = sizeof (*source_array.real7);
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  *cptr = source_array.real8;
	  *size = sizeof (*source_array.real8);
	  break;
#endif

	default:
	  assert ("bad REAL ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (ckt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  *cptr = source_array.complex1;
	  *size = sizeof (*source_array.complex1);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  *cptr = source_array.complex2;
	  *size = sizeof (*source_array.complex2);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  *cptr = source_array.complex3;
	  *size = sizeof (*source_array.complex3);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  *cptr = source_array.complex4;
	  *size = sizeof (*source_array.complex4);
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  *cptr = source_array.complex5;
	  *size = sizeof (*source_array.complex5);
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  *cptr = source_array.complex6;
	  *size = sizeof (*source_array.complex6);
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  *cptr = source_array.complex7;
	  *size = sizeof (*source_array.complex7);
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  *cptr = source_array.complex8;
	  *size = sizeof (*source_array.complex8);
	  break;
#endif

	default:
	  assert ("bad COMPLEX ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ckt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  *cptr = source_array.character1;
	  *size = sizeof (*source_array.character1);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  *cptr = source_array.character2;
	  *size = sizeof (*source_array.character2);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  *cptr = source_array.character3;
	  *size = sizeof (*source_array.character3);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  *cptr = source_array.character4;
	  *size = sizeof (*source_array.character4);
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  *cptr = source_array.character5;
	  *size = sizeof (*source_array.character5);
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  *cptr = source_array.character6;
	  *size = sizeof (*source_array.character6);
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  *cptr = source_array.character7;
	  *size = sizeof (*source_array.character7);
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  *cptr = source_array.character8;
	  *size = sizeof (*source_array.character8);
	  break;
#endif

	default:
	  assert ("bad CHARACTER ckindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad cbasictype" == NULL);
      break;
    }
}

/* ffebld_constantarray_prepare -- Prepare for copy between value and array

   See prototype.

   Like _put, but just returns the pointers to the beginnings of the
   array and the constant and returns the size (the amount of info to
   copy).  The idea is that the caller can use memcpy to accomplish the
   same thing as _put (though slower), or the caller can use a different
   function that swaps bytes, words, etc for a different target machine.
   Also, the type of the array may be different from the type of the
   constant; the array type is used to determine the meaning (scale) of
   the offset field (to calculate the array pointer), the constant type is
   used to determine the constant pointer and the size (amount of info to
   copy).  */

void
ffebld_constantarray_prepare (void **aptr, void **cptr, size_t *size,
       ffebldConstantArray array, ffeinfoBasictype abt, ffeinfoKindtype akt,
		      ffetargetOffset offset, ffebldConstantUnion *constant,
			      ffeinfoBasictype cbt, ffeinfoKindtype ckt)
{
  switch (abt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (akt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  *aptr = array.integer1 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  *aptr = array.integer2 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  *aptr = array.integer3 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  *aptr = array.integer4 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  *aptr = array.integer5 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  *aptr = array.integer6 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  *aptr = array.integer7 + offset;
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  *aptr = array.integer8 + offset;
	  break;
#endif

	default:
	  assert ("bad INTEGER akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (akt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  *aptr = array.logical1 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  *aptr = array.logical2 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  *aptr = array.logical3 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  *aptr = array.logical4 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  *aptr = array.logical5 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  *aptr = array.logical6 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  *aptr = array.logical7 + offset;
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  *aptr = array.logical8 + offset;
	  break;
#endif

	default:
	  assert ("bad LOGICAL akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (akt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  *aptr = array.real1 + offset;
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  *aptr = array.real2 + offset;
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  *aptr = array.real3 + offset;
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  *aptr = array.real4 + offset;
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  *aptr = array.real5 + offset;
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  *aptr = array.real6 + offset;
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  *aptr = array.real7 + offset;
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  *aptr = array.real8 + offset;
	  break;
#endif

	default:
	  assert ("bad REAL akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (akt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  *aptr = array.complex1 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  *aptr = array.complex2 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  *aptr = array.complex3 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  *aptr = array.complex4 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  *aptr = array.complex5 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  *aptr = array.complex6 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  *aptr = array.complex7 + offset;
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  *aptr = array.complex8 + offset;
	  break;
#endif

	default:
	  assert ("bad COMPLEX akindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (akt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  *aptr = array.character1 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  *aptr = array.character2 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  *aptr = array.character3 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  *aptr = array.character4 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  *aptr = array.character5 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  *aptr = array.character6 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  *aptr = array.character7 + offset;
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  *aptr = array.character8 + offset;
	  break;
#endif

	default:
	  assert ("bad CHARACTER akindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad abasictype" == NULL);
      break;
    }

  switch (cbt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (ckt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  *cptr = &constant->integer1;
	  *size = sizeof (constant->integer1);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  *cptr = &constant->integer2;
	  *size = sizeof (constant->integer2);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  *cptr = &constant->integer3;
	  *size = sizeof (constant->integer3);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  *cptr = &constant->integer4;
	  *size = sizeof (constant->integer4);
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  *cptr = &constant->integer5;
	  *size = sizeof (constant->integer5);
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  *cptr = &constant->integer6;
	  *size = sizeof (constant->integer6);
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  *cptr = &constant->integer7;
	  *size = sizeof (constant->integer7);
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  *cptr = &constant->integer8;
	  *size = sizeof (constant->integer8);
	  break;
#endif

	default:
	  assert ("bad INTEGER ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (ckt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  *cptr = &constant->logical1;
	  *size = sizeof (constant->logical1);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  *cptr = &constant->logical2;
	  *size = sizeof (constant->logical2);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  *cptr = &constant->logical3;
	  *size = sizeof (constant->logical3);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  *cptr = &constant->logical4;
	  *size = sizeof (constant->logical4);
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  *cptr = &constant->logical5;
	  *size = sizeof (constant->logical5);
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  *cptr = &constant->logical6;
	  *size = sizeof (constant->logical6);
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  *cptr = &constant->logical7;
	  *size = sizeof (constant->logical7);
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  *cptr = &constant->logical8;
	  *size = sizeof (constant->logical8);
	  break;
#endif

	default:
	  assert ("bad LOGICAL ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ckt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  *cptr = &constant->real1;
	  *size = sizeof (constant->real1);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  *cptr = &constant->real2;
	  *size = sizeof (constant->real2);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  *cptr = &constant->real3;
	  *size = sizeof (constant->real3);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  *cptr = &constant->real4;
	  *size = sizeof (constant->real4);
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  *cptr = &constant->real5;
	  *size = sizeof (constant->real5);
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  *cptr = &constant->real6;
	  *size = sizeof (constant->real6);
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  *cptr = &constant->real7;
	  *size = sizeof (constant->real7);
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  *cptr = &constant->real8;
	  *size = sizeof (constant->real8);
	  break;
#endif

	default:
	  assert ("bad REAL ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (ckt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  *cptr = &constant->complex1;
	  *size = sizeof (constant->complex1);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  *cptr = &constant->complex2;
	  *size = sizeof (constant->complex2);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  *cptr = &constant->complex3;
	  *size = sizeof (constant->complex3);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  *cptr = &constant->complex4;
	  *size = sizeof (constant->complex4);
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  *cptr = &constant->complex5;
	  *size = sizeof (constant->complex5);
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  *cptr = &constant->complex6;
	  *size = sizeof (constant->complex6);
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  *cptr = &constant->complex7;
	  *size = sizeof (constant->complex7);
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  *cptr = &constant->complex8;
	  *size = sizeof (constant->complex8);
	  break;
#endif

	default:
	  assert ("bad COMPLEX ckindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ckt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  *cptr = ffetarget_text_character1 (constant->character1);
	  *size = ffetarget_length_character1 (constant->character1);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  *cptr = ffetarget_text_character2 (constant->character2);
	  *size = ffetarget_length_character2 (constant->character2);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  *cptr = ffetarget_text_character3 (constant->character3);
	  *size = ffetarget_length_character3 (constant->character3);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  *cptr = ffetarget_text_character4 (constant->character4);
	  *size = ffetarget_length_character4 (constant->character4);
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  *cptr = ffetarget_text_character5 (constant->character5);
	  *size = ffetarget_length_character5 (constant->character5);
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  *cptr = ffetarget_text_character6 (constant->character6);
	  *size = ffetarget_length_character6 (constant->character6);
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  *cptr = ffetarget_text_character7 (constant->character7);
	  *size = ffetarget_length_character7 (constant->character7);
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  *cptr = ffetarget_text_character8 (constant->character8);
	  *size = ffetarget_length_character8 (constant->character8);
	  break;
#endif

	default:
	  assert ("bad CHARACTER ckindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad cbasictype" == NULL);
      break;
    }
}

/* ffebld_constantarray_put -- Put a value into an array of constants

   See prototype.  */

void
ffebld_constantarray_put (ffebldConstantArray array, ffeinfoBasictype bt,
   ffeinfoKindtype kt, ffetargetOffset offset, ffebldConstantUnion constant)
{
  switch (bt)
    {
    case FFEINFO_basictypeINTEGER:
      switch (kt)
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  *(array.integer1 + offset) = constant.integer1;
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  *(array.integer2 + offset) = constant.integer2;
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  *(array.integer3 + offset) = constant.integer3;
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  *(array.integer4 + offset) = constant.integer4;
	  break;
#endif

#if FFETARGET_okINTEGER5
	case FFEINFO_kindtypeINTEGER5:
	  *(array.integer5 + offset) = constant.integer5;
	  break;
#endif

#if FFETARGET_okINTEGER6
	case FFEINFO_kindtypeINTEGER6:
	  *(array.integer6 + offset) = constant.integer6;
	  break;
#endif

#if FFETARGET_okINTEGER7
	case FFEINFO_kindtypeINTEGER7:
	  *(array.integer7 + offset) = constant.integer7;
	  break;
#endif

#if FFETARGET_okINTEGER8
	case FFEINFO_kindtypeINTEGER8:
	  *(array.integer8 + offset) = constant.integer8;
	  break;
#endif

	default:
	  assert ("bad INTEGER kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt)
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  *(array.logical1 + offset) = constant.logical1;
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  *(array.logical2 + offset) = constant.logical2;
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  *(array.logical3 + offset) = constant.logical3;
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  *(array.logical4 + offset) = constant.logical4;
	  break;
#endif

#if FFETARGET_okLOGICAL5
	case FFEINFO_kindtypeLOGICAL5:
	  *(array.logical5 + offset) = constant.logical5;
	  break;
#endif

#if FFETARGET_okLOGICAL6
	case FFEINFO_kindtypeLOGICAL6:
	  *(array.logical6 + offset) = constant.logical6;
	  break;
#endif

#if FFETARGET_okLOGICAL7
	case FFEINFO_kindtypeLOGICAL7:
	  *(array.logical7 + offset) = constant.logical7;
	  break;
#endif

#if FFETARGET_okLOGICAL8
	case FFEINFO_kindtypeLOGICAL8:
	  *(array.logical8 + offset) = constant.logical8;
	  break;
#endif

	default:
	  assert ("bad LOGICAL kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt)
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  *(array.real1 + offset) = constant.real1;
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  *(array.real2 + offset) = constant.real2;
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  *(array.real3 + offset) = constant.real3;
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  *(array.real4 + offset) = constant.real4;
	  break;
#endif

#if FFETARGET_okREAL5
	case FFEINFO_kindtypeREAL5:
	  *(array.real5 + offset) = constant.real5;
	  break;
#endif

#if FFETARGET_okREAL6
	case FFEINFO_kindtypeREAL6:
	  *(array.real6 + offset) = constant.real6;
	  break;
#endif

#if FFETARGET_okREAL7
	case FFEINFO_kindtypeREAL7:
	  *(array.real7 + offset) = constant.real7;
	  break;
#endif

#if FFETARGET_okREAL8
	case FFEINFO_kindtypeREAL8:
	  *(array.real8 + offset) = constant.real8;
	  break;
#endif

	default:
	  assert ("bad REAL kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  *(array.complex1 + offset) = constant.complex1;
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  *(array.complex2 + offset) = constant.complex2;
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  *(array.complex3 + offset) = constant.complex3;
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  *(array.complex4 + offset) = constant.complex4;
	  break;
#endif

#if FFETARGET_okCOMPLEX5
	case FFEINFO_kindtypeREAL5:
	  *(array.complex5 + offset) = constant.complex5;
	  break;
#endif

#if FFETARGET_okCOMPLEX6
	case FFEINFO_kindtypeREAL6:
	  *(array.complex6 + offset) = constant.complex6;
	  break;
#endif

#if FFETARGET_okCOMPLEX7
	case FFEINFO_kindtypeREAL7:
	  *(array.complex7 + offset) = constant.complex7;
	  break;
#endif

#if FFETARGET_okCOMPLEX8
	case FFEINFO_kindtypeREAL8:
	  *(array.complex8 + offset) = constant.complex8;
	  break;
#endif

	default:
	  assert ("bad COMPLEX kindtype" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (kt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  memcpy (array.character1 + offset,
		  ffetarget_text_character1 (constant.character1),
		  ffetarget_length_character1 (constant.character1));
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  memcpy (array.character2 + offset,
		  ffetarget_text_character2 (constant.character2),
		  ffetarget_length_character2 (constant.character2));
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  memcpy (array.character3 + offset,
		  ffetarget_text_character3 (constant.character3),
		  ffetarget_length_character3 (constant.character3));
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  memcpy (array.character4 + offset,
		  ffetarget_text_character4 (constant.character4),
		  ffetarget_length_character4 (constant.character4));
	  break;
#endif

#if FFETARGET_okCHARACTER5
	case FFEINFO_kindtypeCHARACTER5:
	  memcpy (array.character5 + offset,
		  ffetarget_text_character5 (constant.character5),
		  ffetarget_length_character5 (constant.character5));
	  break;
#endif

#if FFETARGET_okCHARACTER6
	case FFEINFO_kindtypeCHARACTER6:
	  memcpy (array.character6 + offset,
		  ffetarget_text_character6 (constant.character6),
		  ffetarget_length_character6 (constant.character6));
	  break;
#endif

#if FFETARGET_okCHARACTER7
	case FFEINFO_kindtypeCHARACTER7:
	  memcpy (array.character7 + offset,
		  ffetarget_text_character7 (constant.character7),
		  ffetarget_length_character7 (constant.character7));
	  break;
#endif

#if FFETARGET_okCHARACTER8
	case FFEINFO_kindtypeCHARACTER8:
	  memcpy (array.character8 + offset,
		  ffetarget_text_character8 (constant.character8),
		  ffetarget_length_character8 (constant.character8));
	  break;
#endif

	default:
	  assert ("bad CHARACTER kindtype" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad basictype" == NULL);
      break;
    }
}

/* ffebld_init_0 -- Initialize the module

   ffebld_init_0();  */

void
ffebld_init_0 ()
{
  assert (FFEBLD_op == ARRAY_SIZE (ffebld_op_string_));
  assert (FFEBLD_op == ARRAY_SIZE (ffebld_arity_op_));
}

/* ffebld_init_1 -- Initialize the module for a file

   ffebld_init_1();  */

void
ffebld_init_1 ()
{
#if FFEBLD_whereconstCURRENT_ == FFEBLD_whereconstFILE_
  int i;

#if FFETARGET_okCHARACTER1
  ffebld_constant_character1_ = NULL;
#endif
#if FFETARGET_okCHARACTER2
  ffebld_constant_character2_ = NULL;
#endif
#if FFETARGET_okCHARACTER3
  ffebld_constant_character3_ = NULL;
#endif
#if FFETARGET_okCHARACTER4
  ffebld_constant_character4_ = NULL;
#endif
#if FFETARGET_okCHARACTER5
  ffebld_constant_character5_ = NULL;
#endif
#if FFETARGET_okCHARACTER6
  ffebld_constant_character6_ = NULL;
#endif
#if FFETARGET_okCHARACTER7
  ffebld_constant_character7_ = NULL;
#endif
#if FFETARGET_okCHARACTER8
  ffebld_constant_character8_ = NULL;
#endif
#if FFETARGET_okCOMPLEX1
  ffebld_constant_complex1_ = NULL;
#endif
#if FFETARGET_okCOMPLEX2
  ffebld_constant_complex2_ = NULL;
#endif
#if FFETARGET_okCOMPLEX3
  ffebld_constant_complex3_ = NULL;
#endif
#if FFETARGET_okCOMPLEX4
  ffebld_constant_complex4_ = NULL;
#endif
#if FFETARGET_okCOMPLEX5
  ffebld_constant_complex5_ = NULL;
#endif
#if FFETARGET_okCOMPLEX6
  ffebld_constant_complex6_ = NULL;
#endif
#if FFETARGET_okCOMPLEX7
  ffebld_constant_complex7_ = NULL;
#endif
#if FFETARGET_okCOMPLEX8
  ffebld_constant_complex8_ = NULL;
#endif
#if FFETARGET_okINTEGER1
  ffebld_constant_integer1_ = NULL;
#endif
#if FFETARGET_okINTEGER2
  ffebld_constant_integer2_ = NULL;
#endif
#if FFETARGET_okINTEGER3
  ffebld_constant_integer3_ = NULL;
#endif
#if FFETARGET_okINTEGER4
  ffebld_constant_integer4_ = NULL;
#endif
#if FFETARGET_okINTEGER5
  ffebld_constant_integer5_ = NULL;
#endif
#if FFETARGET_okINTEGER6
  ffebld_constant_integer6_ = NULL;
#endif
#if FFETARGET_okINTEGER7
  ffebld_constant_integer7_ = NULL;
#endif
#if FFETARGET_okINTEGER8
  ffebld_constant_integer8_ = NULL;
#endif
#if FFETARGET_okLOGICAL1
  ffebld_constant_logical1_ = NULL;
#endif
#if FFETARGET_okLOGICAL2
  ffebld_constant_logical2_ = NULL;
#endif
#if FFETARGET_okLOGICAL3
  ffebld_constant_logical3_ = NULL;
#endif
#if FFETARGET_okLOGICAL4
  ffebld_constant_logical4_ = NULL;
#endif
#if FFETARGET_okLOGICAL5
  ffebld_constant_logical5_ = NULL;
#endif
#if FFETARGET_okLOGICAL6
  ffebld_constant_logical6_ = NULL;
#endif
#if FFETARGET_okLOGICAL7
  ffebld_constant_logical7_ = NULL;
#endif
#if FFETARGET_okLOGICAL8
  ffebld_constant_logical8_ = NULL;
#endif
#if FFETARGET_okREAL1
  ffebld_constant_real1_ = NULL;
#endif
#if FFETARGET_okREAL2
  ffebld_constant_real2_ = NULL;
#endif
#if FFETARGET_okREAL3
  ffebld_constant_real3_ = NULL;
#endif
#if FFETARGET_okREAL4
  ffebld_constant_real4_ = NULL;
#endif
#if FFETARGET_okREAL5
  ffebld_constant_real5_ = NULL;
#endif
#if FFETARGET_okREAL6
  ffebld_constant_real6_ = NULL;
#endif
#if FFETARGET_okREAL7
  ffebld_constant_real7_ = NULL;
#endif
#if FFETARGET_okREAL8
  ffebld_constant_real8_ = NULL;
#endif
  ffebld_constant_hollerith_ = NULL;
  for (i = FFEBLD_constTYPELESS_FIRST; i <= FFEBLD_constTYPELESS_LAST; ++i)
    ffebld_constant_typeless_[i - FFEBLD_constTYPELESS_FIRST] = NULL;
#endif
}

/* ffebld_init_2 -- Initialize the module

   ffebld_init_2();  */

void
ffebld_init_2 ()
{
#if FFEBLD_whereconstCURRENT_ == FFEBLD_whereconstPROGUNIT_
  int i;
#endif

  ffebld_pool_stack_.next = NULL;
  ffebld_pool_stack_.pool = ffe_pool_program_unit ();
#if FFEBLD_whereconstCURRENT_ == FFEBLD_whereconstPROGUNIT_
#if FFETARGET_okCHARACTER1
  ffebld_constant_character1_ = NULL;
#endif
#if FFETARGET_okCHARACTER2
  ffebld_constant_character2_ = NULL;
#endif
#if FFETARGET_okCHARACTER3
  ffebld_constant_character3_ = NULL;
#endif
#if FFETARGET_okCHARACTER4
  ffebld_constant_character4_ = NULL;
#endif
#if FFETARGET_okCHARACTER5
  ffebld_constant_character5_ = NULL;
#endif
#if FFETARGET_okCHARACTER6
  ffebld_constant_character6_ = NULL;
#endif
#if FFETARGET_okCHARACTER7
  ffebld_constant_character7_ = NULL;
#endif
#if FFETARGET_okCHARACTER8
  ffebld_constant_character8_ = NULL;
#endif
#if FFETARGET_okCOMPLEX1
  ffebld_constant_complex1_ = NULL;
#endif
#if FFETARGET_okCOMPLEX2
  ffebld_constant_complex2_ = NULL;
#endif
#if FFETARGET_okCOMPLEX3
  ffebld_constant_complex3_ = NULL;
#endif
#if FFETARGET_okCOMPLEX4
  ffebld_constant_complex4_ = NULL;
#endif
#if FFETARGET_okCOMPLEX5
  ffebld_constant_complex5_ = NULL;
#endif
#if FFETARGET_okCOMPLEX6
  ffebld_constant_complex6_ = NULL;
#endif
#if FFETARGET_okCOMPLEX7
  ffebld_constant_complex7_ = NULL;
#endif
#if FFETARGET_okCOMPLEX8
  ffebld_constant_complex8_ = NULL;
#endif
#if FFETARGET_okINTEGER1
  ffebld_constant_integer1_ = NULL;
#endif
#if FFETARGET_okINTEGER2
  ffebld_constant_integer2_ = NULL;
#endif
#if FFETARGET_okINTEGER3
  ffebld_constant_integer3_ = NULL;
#endif
#if FFETARGET_okINTEGER4
  ffebld_constant_integer4_ = NULL;
#endif
#if FFETARGET_okINTEGER5
  ffebld_constant_integer5_ = NULL;
#endif
#if FFETARGET_okINTEGER6
  ffebld_constant_integer6_ = NULL;
#endif
#if FFETARGET_okINTEGER7
  ffebld_constant_integer7_ = NULL;
#endif
#if FFETARGET_okINTEGER8
  ffebld_constant_integer8_ = NULL;
#endif
#if FFETARGET_okLOGICAL1
  ffebld_constant_logical1_ = NULL;
#endif
#if FFETARGET_okLOGICAL2
  ffebld_constant_logical2_ = NULL;
#endif
#if FFETARGET_okLOGICAL3
  ffebld_constant_logical3_ = NULL;
#endif
#if FFETARGET_okLOGICAL4
  ffebld_constant_logical4_ = NULL;
#endif
#if FFETARGET_okLOGICAL5
  ffebld_constant_logical5_ = NULL;
#endif
#if FFETARGET_okLOGICAL6
  ffebld_constant_logical6_ = NULL;
#endif
#if FFETARGET_okLOGICAL7
  ffebld_constant_logical7_ = NULL;
#endif
#if FFETARGET_okLOGICAL8
  ffebld_constant_logical8_ = NULL;
#endif
#if FFETARGET_okREAL1
  ffebld_constant_real1_ = NULL;
#endif
#if FFETARGET_okREAL2
  ffebld_constant_real2_ = NULL;
#endif
#if FFETARGET_okREAL3
  ffebld_constant_real3_ = NULL;
#endif
#if FFETARGET_okREAL4
  ffebld_constant_real4_ = NULL;
#endif
#if FFETARGET_okREAL5
  ffebld_constant_real5_ = NULL;
#endif
#if FFETARGET_okREAL6
  ffebld_constant_real6_ = NULL;
#endif
#if FFETARGET_okREAL7
  ffebld_constant_real7_ = NULL;
#endif
#if FFETARGET_okREAL8
  ffebld_constant_real8_ = NULL;
#endif
  ffebld_constant_hollerith_ = NULL;
  for (i = FFEBLD_constTYPELESS_FIRST; i <= FFEBLD_constTYPELESS_LAST; ++i)
    ffebld_constant_typeless_[i - FFEBLD_constTYPELESS_FIRST] = NULL;
#endif
}

/* ffebld_list_length -- Return # of opITEMs in list

   ffebld list;	 // Must be NULL or opITEM
   ffebldListLength length;
   length = ffebld_list_length(list);

   Returns 0 if list is NULL, 1 if it's ffebld_trail is NULL, and so on.  */

ffebldListLength
ffebld_list_length (ffebld list)
{
  ffebldListLength length;

  for (length = 0; list != NULL; ++length, list = ffebld_trail (list))
    ;

  return length;
}

/* ffebld_new_accter -- Create an ffebld object that is an array

   ffebld x;
   ffebldConstantArray a;
   ffebit b;
   x = ffebld_new_accter(a,b);	*/

ffebld
ffebld_new_accter (ffebldConstantArray a, ffebit b)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opACCTER;
  x->u.accter.array = a;
  x->u.accter.bits = b;
  x->u.accter.pad = 0;
  return x;
}

/* ffebld_new_arrter -- Create an ffebld object that is an array

   ffebld x;
   ffebldConstantArray a;
   ffetargetOffset size;
   x = ffebld_new_arrter(a,size);  */

ffebld
ffebld_new_arrter (ffebldConstantArray a, ffetargetOffset size)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opARRTER;
  x->u.arrter.array = a;
  x->u.arrter.size = size;
  x->u.arrter.pad = 0;
  return x;
}

/* ffebld_new_conter_with_orig -- Create an ffebld object that is a constant

   ffebld x;
   ffebldConstant c;
   x = ffebld_new_conter_with_orig(c,NULL);  */

ffebld
ffebld_new_conter_with_orig (ffebldConstant c, ffebld o)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opCONTER;
  x->u.conter.expr = c;
  x->u.conter.orig = o;
  x->u.conter.pad = 0;
  return x;
}

/* ffebld_new_item -- Create an ffebld item object

   ffebld x,y,z;
   x = ffebld_new_item(y,z);  */

ffebld
ffebld_new_item (ffebld head, ffebld trail)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opITEM;
  x->u.item.head = head;
  x->u.item.trail = trail;
#ifdef FFECOM_itemHOOK
  x->u.item.hook = FFECOM_itemNULL;
#endif
  return x;
}

/* ffebld_new_labter -- Create an ffebld object that is a label

   ffebld x;
   ffelab l;
   x = ffebld_new_labter(c);  */

ffebld
ffebld_new_labter (ffelab l)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opLABTER;
  x->u.labter = l;
  return x;
}

/* ffebld_new_labtok -- Create object that is a label's NUMBER token

   ffebld x;
   ffelexToken t;
   x = ffebld_new_labter(c);

   Like the other ffebld_new_ functions, the
   supplied argument is stored exactly as is: ffelex_token_use is NOT
   called, so the token is "consumed", if one is indeed supplied (it may
   be NULL).  */

ffebld
ffebld_new_labtok (ffelexToken t)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opLABTOK;
  x->u.labtok = t;
  return x;
}

/* ffebld_new_none -- Create an ffebld object with no arguments

   ffebld x;
   x = ffebld_new_none(FFEBLD_opWHATEVER);  */

ffebld
ffebld_new_none (ffebldOp o)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = o;
  return x;
}

/* ffebld_new_one -- Create an ffebld object with one argument

   ffebld x,y;
   x = ffebld_new_one(FFEBLD_opWHATEVER,y);  */

ffebld
ffebld_new_one (ffebldOp o, ffebld left)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = o;
  x->u.nonter.left = left;
#ifdef FFECOM_nonterHOOK
  x->u.nonter.hook = FFECOM_nonterNULL;
#endif
  return x;
}

/* ffebld_new_symter -- Create an ffebld object that is a symbol

   ffebld x;
   ffesymbol s;
   ffeintrinGen gen;	// Generic intrinsic id, if any
   ffeintrinSpec spec;	// Specific intrinsic id, if any
   ffeintrinImp imp;	// Implementation intrinsic id, if any
   x = ffebld_new_symter (s, gen, spec, imp);  */

ffebld
ffebld_new_symter (ffesymbol s, ffeintrinGen gen, ffeintrinSpec spec,
		   ffeintrinImp imp)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = FFEBLD_opSYMTER;
  x->u.symter.symbol = s;
  x->u.symter.generic = gen;
  x->u.symter.specific = spec;
  x->u.symter.implementation = imp;
  x->u.symter.do_iter = FALSE;
  return x;
}

/* ffebld_new_two -- Create an ffebld object with two arguments

   ffebld x,y,z;
   x = ffebld_new_two(FFEBLD_opWHATEVER,y,z);  */

ffebld
ffebld_new_two (ffebldOp o, ffebld left, ffebld right)
{
  ffebld x;

  x = ffebld_new ();
#if FFEBLD_BLANK_
  *x = ffebld_blank_;
#endif
  x->op = o;
  x->u.nonter.left = left;
  x->u.nonter.right = right;
#ifdef FFECOM_nonterHOOK
  x->u.nonter.hook = FFECOM_nonterNULL;
#endif
  return x;
}

/* ffebld_pool_pop -- Pop ffebld's pool stack

   ffebld_pool_pop();  */

void
ffebld_pool_pop ()
{
  ffebldPoolstack_ ps;

  assert (ffebld_pool_stack_.next != NULL);
  ps = ffebld_pool_stack_.next;
  ffebld_pool_stack_.next = ps->next;
  ffebld_pool_stack_.pool = ps->pool;
  malloc_kill_ks (malloc_pool_image (), ps, sizeof (*ps));
}

/* ffebld_pool_push -- Push ffebld's pool stack

   ffebld_pool_push();	*/

void
ffebld_pool_push (mallocPool pool)
{
  ffebldPoolstack_ ps;

  ps = malloc_new_ks (malloc_pool_image (), "Pool stack", sizeof (*ps));
  ps->next = ffebld_pool_stack_.next;
  ps->pool = ffebld_pool_stack_.pool;
  ffebld_pool_stack_.next = ps;
  ffebld_pool_stack_.pool = pool;
}

/* ffebld_op_string -- Return short string describing op

   ffebldOp o;
   ffebld_op_string(o);

   Returns a short string (uppercase) containing the name of the op.  */

const char *
ffebld_op_string (ffebldOp o)
{
  if (o >= ARRAY_SIZE (ffebld_op_string_))
    return "?\?\?";
  return ffebld_op_string_[o];
}

/* ffebld_size_max -- Return maximum possible size of CHARACTER-type expr

   ffetargetCharacterSize sz;
   ffebld b;
   sz = ffebld_size_max (b);

   Like ffebld_size_known, but if that would return NONE and the expression
   is opSUBSTR, opCONVERT, opPAREN, or opCONCATENATE, returns ffebld_size_max
   of the subexpression(s).  */

ffetargetCharacterSize
ffebld_size_max (ffebld b)
{
  ffetargetCharacterSize sz;

recurse:			/* :::::::::::::::::::: */

  sz = ffebld_size_known (b);

  if (sz != FFETARGET_charactersizeNONE)
    return sz;

  switch (ffebld_op (b))
    {
    case FFEBLD_opSUBSTR:
    case FFEBLD_opCONVERT:
    case FFEBLD_opPAREN:
      b = ffebld_left (b);
      goto recurse;		/* :::::::::::::::::::: */

    case FFEBLD_opCONCATENATE:
      sz = ffebld_size_max (ffebld_left (b))
	+ ffebld_size_max (ffebld_right (b));
      return sz;

    default:
      return sz;
    }
}
