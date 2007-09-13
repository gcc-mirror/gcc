/* Header file for dfp-bit.c.
   Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef _DFPBIT_H
#define _DFPBIT_H

#include <float.h>
#include <fenv.h>
#include <decRound.h>
#include <decExcept.h>
#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"

#ifndef LIBGCC2_WORDS_BIG_ENDIAN
#define LIBGCC2_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN
#endif

#ifndef LIBGCC2_FLOAT_WORDS_BIG_ENDIAN
#define LIBGCC2_FLOAT_WORDS_BIG_ENDIAN LIBGCC2_WORDS_BIG_ENDIAN
#endif

#ifndef LIBGCC2_LONG_DOUBLE_TYPE_SIZE
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE LONG_DOUBLE_TYPE_SIZE
#endif

/* We need to know the size of long double that the C library supports.
   Don't use LIBGCC2_HAS_XF_MODE or LIBGCC2_HAS_TF_MODE here because
   some targets set both of those.  */

#define LONG_DOUBLE_HAS_XF_MODE \
  (BITS_PER_UNIT == 8 && LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 80)

#define LONG_DOUBLE_HAS_TF_MODE \
  (BITS_PER_UNIT == 8 && LIBGCC2_LONG_DOUBLE_TYPE_SIZE == 128)

/* Depending on WIDTH, define a number of macros:

   DFP_C_TYPE: type of the arguments to the libgcc functions;
	(eg _Decimal32)

   IEEE_TYPE: the corresponding (encoded) IEEE754R type;
	(eg decimal32)
   
   TO_INTERNAL: the name of the decNumber function to convert an
   encoded value into the decNumber internal representation;

   TO_ENCODED: the name of the decNumber function to convert an
   internally represented decNumber into the encoded
   representation.

   FROM_STRING: the name of the decNumber function to read an
   encoded value from a string.

   TO_STRING: the name of the decNumber function to write an
   encoded value to a string.  */

#if WIDTH == 32
#define DFP_C_TYPE	_Decimal32
#define IEEE_TYPE	decimal32
#define HOST_TO_IEEE	__host_to_ieee_32
#define IEEE_TO_HOST	__ieee_to_host_32
#define TO_INTERNAL	__decimal32ToNumber
#define TO_ENCODED	__decimal32FromNumber
#define FROM_STRING	__decimal32FromString
#define TO_STRING	__decimal32ToString
#elif WIDTH == 64
#define DFP_C_TYPE	_Decimal64
#define IEEE_TYPE	decimal64
#define HOST_TO_IEEE	__host_to_ieee_64
#define IEEE_TO_HOST	__ieee_to_host_64
#define TO_INTERNAL	__decimal64ToNumber
#define TO_ENCODED	__decimal64FromNumber
#define FROM_STRING	__decimal64FromString
#define TO_STRING	__decimal64ToString
#elif WIDTH == 128
#define DFP_C_TYPE	_Decimal128
#define IEEE_TYPE	decimal128
#define HOST_TO_IEEE	__host_to_ieee_128
#define IEEE_TO_HOST	__ieee_to_host_128
#define TO_INTERNAL	__decimal128ToNumber
#define TO_ENCODED	__decimal128FromNumber
#define FROM_STRING	__decimal128FromString
#define TO_STRING	__decimal128ToString
#else
#error invalid decimal float word width
#endif

/* We define __DEC_EVAL_METHOD__ to 2, saying that we evaluate all
   operations and constants to the range and precision of the _Decimal128
   type.  Make it so.  */
#if WIDTH == 32
#define CONTEXT_INIT DEC_INIT_DECIMAL32
#elif WIDTH == 64
#define CONTEXT_INIT DEC_INIT_DECIMAL64
#elif WIDTH == 128
#define CONTEXT_INIT DEC_INIT_DECIMAL128
#endif

#ifndef DFP_INIT_ROUNDMODE
#define DFP_INIT_ROUNDMODE(A) A = DEC_ROUND_HALF_EVEN
#endif

#ifdef DFP_EXCEPTIONS_ENABLED
/* Return IEEE exception flags based on decNumber status flags.  */
#define DFP_IEEE_FLAGS(DEC_FLAGS) __extension__			\
({int _fe_flags = 0;						\
  if ((dec_flags & DEC_IEEE_854_Division_by_zero) != 0)		\
    _fe_flags |= FE_DIVBYZERO;					\
  if ((dec_flags & DEC_IEEE_854_Inexact) != 0)			\
    _fe_flags |= FE_INEXACT;					\
  if ((dec_flags & DEC_IEEE_854_Invalid_operation) != 0)	\
    _fe_flags |= FE_INVALID;					\
  if ((dec_flags & DEC_IEEE_854_Overflow) != 0)			\
    _fe_flags |= FE_OVERFLOW;					\
  if ((dec_flags & DEC_IEEE_854_Underflow) != 0)		\
    _fe_flags |= FE_UNDERFLOW;					\
  _fe_flags; })
#else
#define DFP_EXCEPTIONS_ENABLED 0
#define DFP_IEEE_FLAGS(A) 0
#define DFP_HANDLE_EXCEPTIONS(A) do {} while (0)
#endif

/* Conversions between different decimal float types use WIDTH_TO to
   determine additional macros to define.  */

#if defined (L_dd_to_sd) || defined (L_td_to_sd)
#define WIDTH_TO 32
#elif defined (L_sd_to_dd) || defined (L_td_to_dd)
#define WIDTH_TO 64
#elif defined (L_sd_to_td) || defined (L_dd_to_td)
#define WIDTH_TO 128
#endif

/* If WIDTH_TO is defined, define additional macros:

   DFP_C_TYPE_TO: type of the result of dfp to dfp conversion.

   IEEE_TYPE_TO: the corresponding (encoded) IEEE754R type.

   TO_ENCODED_TO: the name of the decNumber function to convert an
   internally represented decNumber into the encoded representation
   for the destination.  */

#if WIDTH_TO == 32
#define DFP_C_TYPE_TO	_Decimal32
#define IEEE_TYPE_TO	decimal32
#define TO_ENCODED_TO	__decimal32FromNumber
#define IEEE_TO_HOST_TO __ieee_to_host_32
#elif WIDTH_TO == 64
#define DFP_C_TYPE_TO	_Decimal64
#define IEEE_TYPE_TO	decimal64
#define TO_ENCODED_TO	__decimal64FromNumber
#define IEEE_TO_HOST_TO __ieee_to_host_64
#elif WIDTH_TO == 128
#define DFP_C_TYPE_TO	_Decimal128
#define IEEE_TYPE_TO	decimal128
#define TO_ENCODED_TO	__decimal128FromNumber
#define IEEE_TO_HOST_TO __ieee_to_host_128
#endif

/* Conversions between decimal float types and integral types use INT_KIND
   to determine the data type and C functions to use.  */

#if defined (L_sd_to_si) || defined (L_dd_to_si) || defined (L_td_to_si)  \
   || defined (L_si_to_sd) || defined (L_si_to_dd) || defined (L_si_to_td)
#define INT_KIND 1
#elif defined (L_sd_to_di) || defined (L_dd_to_di) || defined (L_td_to_di) \
   || defined (L_di_to_sd) || defined (L_di_to_dd) || defined (L_di_to_td)
#define INT_KIND 2
#elif defined (L_sd_to_usi) || defined (L_dd_to_usi) || defined (L_td_to_usi) \
   || defined (L_usi_to_sd) || defined (L_usi_to_dd) || defined (L_usi_to_td)
#define INT_KIND 3
#elif defined (L_sd_to_udi) || defined (L_dd_to_udi) || defined (L_td_to_udi) \
   || defined (L_udi_to_sd) || defined (L_udi_to_dd) || defined (L_udi_to_td)
#define INT_KIND 4
#endif

/*  If INT_KIND is defined, define additional macros:

    INT_TYPE: The integer data type.

    INT_FMT: The format string for writing the integer to a string.

    CAST_FOR_FMT: Cast variable of INT_KIND to C type for sprintf.
    This works for ILP32 and LP64, won't for other type size systems.

    STR_TO_INT: The function to read the integer from a string.  */

#if INT_KIND == 1
#define INT_TYPE SItype
#define INT_FMT "%d"
#define CAST_FOR_FMT(A) (int)A
#define STR_TO_INT strtol
#elif INT_KIND == 2
#define INT_TYPE DItype
#define INT_FMT "%lld"
#define CAST_FOR_FMT(A) (long long)A
#define STR_TO_INT strtoll
#elif INT_KIND == 3
#define INT_TYPE USItype
#define INT_FMT "%u"
#define CAST_FOR_FMT(A) (unsigned int)A
#define STR_TO_INT strtoul
#elif INT_KIND == 4
#define INT_TYPE UDItype
#define INT_FMT "%llu"
#define CAST_FOR_FMT(A) (unsigned long long)A
#define STR_TO_INT strtoull
#endif

/* Conversions between decimal float types and binary float types use
   BFP_KIND to determine the data type and C functions to use.  */

#if defined (L_sd_to_sf) || defined (L_dd_to_sf) || defined (L_td_to_sf) \
 || defined (L_sf_to_sd) || defined (L_sf_to_dd) || defined (L_sf_to_td)
#define BFP_KIND 1
#elif defined (L_sd_to_df) || defined (L_dd_to_df ) || defined (L_td_to_df) \
 ||   defined (L_df_to_sd) || defined (L_df_to_dd) || defined (L_df_to_td)
#define BFP_KIND 2
#elif defined (L_sd_to_xf) || defined (L_dd_to_xf ) || defined (L_td_to_xf) \
 ||   defined (L_xf_to_sd) || defined (L_xf_to_dd) || defined (L_xf_to_td)
#define BFP_KIND 3
#elif defined (L_sd_to_tf) || defined (L_dd_to_tf) || defined (L_td_to_tf) \
 ||   defined (L_tf_to_sd) || defined (L_tf_to_dd) || defined (L_tf_to_td)
#define BFP_KIND 4
#endif

/*  If BFP_KIND is defined, define additional macros:

    BFP_TYPE: The binary floating point data type.

    BFP_FMT: The format string for writing the value to a string.
    The number of decimal digits printed is
       ceil (nbits / log2 (10.) + 1)
    as described in David Matula's CACM 19(3) 716-723 June 1968 paper.

    BFP_VIA_TYPE: Type to which to cast a variable of BPF_TYPE for a
    call to sprintf.
    
    STR_TO_BFP: The function to read the value from a string.  */

#if BFP_KIND == 1
#define BFP_TYPE SFtype
#define BFP_FMT "%.9e"
#define BFP_VIA_TYPE double
#define STR_TO_BFP strtof

#elif BFP_KIND == 2
#define BFP_TYPE DFtype
#define BFP_FMT "%.17e"
#define BFP_VIA_TYPE double
#define STR_TO_BFP strtod

#elif BFP_KIND == 3
#if LONG_DOUBLE_HAS_XF_MODE
#define BFP_TYPE XFtype
#define BFP_FMT "%.21Le"
#define BFP_VIA_TYPE long double
#define STR_TO_BFP strtold
#endif /* LONG_DOUBLE_HAS_XF_MODE */

#elif BFP_KIND == 4
#if LONG_DOUBLE_HAS_TF_MODE
#define BFP_TYPE TFtype
#if LDBL_MANT_DIG == 106
#define BFP_FMT "%.33Le"
#elif LDBL_MANT_DIG == 113
#define BFP_FMT "%.36Le"
#else
#error "unknown long double size, cannot define BFP_FMT"
#endif /* LDBL_MANT_DIG */
#define STR_TO_BFP strtold
#define BFP_VIA_TYPE long double
#endif /* LONG_DOUBLE_HAS_TF_MODE */

#endif /* BFP_KIND */

#if WIDTH == 128 || WIDTH_TO == 128
#include "decimal128.h"
#include "decQuad.h"
#endif
#if WIDTH == 64 || WIDTH_TO == 64
#include "decimal64.h"
#include "decDouble.h"
#endif
#if WIDTH == 32 || WIDTH_TO == 32
#include "decimal32.h"
#include "decSingle.h"
#endif
#include "decNumber.h"

/* Names of arithmetic functions.  */

#if ENABLE_DECIMAL_BID_FORMAT
#define DPD_BID_NAME(DPD,BID) BID
#else
#define DPD_BID_NAME(DPD,BID) DPD
#endif

#if WIDTH == 32
#define DFP_ADD		DPD_BID_NAME(__dpd_addsd3,__bid_addsd3)
#define DFP_SUB		DPD_BID_NAME(__dpd_subsd3,__bid_subsd3)
#define DFP_MULTIPLY	DPD_BID_NAME(__dpd_mulsd3,__bid_mulsd3)
#define DFP_DIVIDE	DPD_BID_NAME(__dpd_divsd3,__bid_divsd3)
#define DFP_EQ		DPD_BID_NAME(__dpd_eqsd2,__bid_eqsd2)
#define DFP_NE		DPD_BID_NAME(__dpd_nesd2,__bid_nesd2)
#define DFP_LT		DPD_BID_NAME(__dpd_ltsd2,__bid_ltsd2)
#define DFP_GT		DPD_BID_NAME(__dpd_gtsd2,__bid_gtsd2)
#define DFP_LE		DPD_BID_NAME(__dpd_lesd2,__bid_lesd2)
#define DFP_GE		DPD_BID_NAME(__dpd_gesd2,__bid_gesd2)
#define DFP_UNORD	DPD_BID_NAME(__dpd_unordsd2,__bid_unordsd2)
#elif WIDTH == 64
#define DFP_ADD		DPD_BID_NAME(__dpd_adddd3,__bid_adddd3)
#define DFP_SUB		DPD_BID_NAME(__dpd_subdd3,__bid_subdd3)
#define DFP_MULTIPLY	DPD_BID_NAME(__dpd_muldd3,__bid_muldd3)
#define DFP_DIVIDE	DPD_BID_NAME(__dpd_divdd3,__bid_divdd3)
#define DFP_EQ		DPD_BID_NAME(__dpd_eqdd2,__bid_eqdd2)
#define DFP_NE		DPD_BID_NAME(__dpd_nedd2,__bid_nedd2)
#define DFP_LT		DPD_BID_NAME(__dpd_ltdd2,__bid_ltdd2)
#define DFP_GT		DPD_BID_NAME(__dpd_gtdd2,__bid_gtdd2)
#define DFP_LE		DPD_BID_NAME(__dpd_ledd2,__bid_ledd2)
#define DFP_GE		DPD_BID_NAME(__dpd_gedd2,__bid_gedd2)
#define DFP_UNORD	DPD_BID_NAME(__dpd_unorddd2,__bid_unorddd2)
#elif WIDTH == 128
#define DFP_ADD		DPD_BID_NAME(__dpd_addtd3,__bid_addtd3)
#define DFP_SUB		DPD_BID_NAME(__dpd_subtd3,__bid_subtd3)
#define DFP_MULTIPLY	DPD_BID_NAME(__dpd_multd3,__bid_multd3)
#define DFP_DIVIDE	DPD_BID_NAME(__dpd_divtd3,__bid_divtd3)
#define DFP_EQ		DPD_BID_NAME(__dpd_eqtd2,__bid_eqtd2)
#define DFP_NE		DPD_BID_NAME(__dpd_netd2,__bid_netd2)
#define DFP_LT		DPD_BID_NAME(__dpd_lttd2,__bid_lttd2)
#define DFP_GT		DPD_BID_NAME(__dpd_gttd2,__bid_gttd2)
#define DFP_LE		DPD_BID_NAME(__dpd_letd2,__bid_letd2)
#define DFP_GE		DPD_BID_NAME(__dpd_getd2,__bid_getd2)
#define DFP_UNORD	DPD_BID_NAME(__dpd_unordtd2,__bid_unordtd2)
#endif

/* Names of decNumber functions for DPD arithmetic.  */

#if WIDTH == 32
#define decFloat		decDouble
#define DFP_BINARY_OP		d32_binary_op
#define DFP_COMPARE_OP		d32_compare_op
#define DEC_FLOAT_ADD		decDoubleAdd
#define DEC_FLOAT_SUBTRACT	decDoubleSubtract
#define DEC_FLOAT_MULTIPLY	decDoubleMultiply
#define DEC_FLOAT_DIVIDE	decDoubleDivide
#define DEC_FLOAT_COMPARE	decDoubleCompare
#define DEC_FLOAT_IS_ZERO	decDoubleIsZero
#define DEC_FLOAT_IS_NAN	decDoubleIsNaN
#define DEC_FLOAT_IS_SIGNED	decDoubleIsSigned
#elif WIDTH == 64
#define DFP_BINARY_OP		dnn_binary_op
#define DFP_COMPARE_OP		dnn_compare_op
#define decFloat		decDouble
#define DEC_FLOAT_ADD		decDoubleAdd
#define DEC_FLOAT_SUBTRACT	decDoubleSubtract
#define DEC_FLOAT_MULTIPLY	decDoubleMultiply
#define DEC_FLOAT_DIVIDE	decDoubleDivide
#define DEC_FLOAT_COMPARE	decDoubleCompare
#define DEC_FLOAT_IS_ZERO	decDoubleIsZero
#define DEC_FLOAT_IS_NAN	decDoubleIsNaN
#define DEC_FLOAT_IS_SIGNED	decDoubleIsSigned
#elif WIDTH == 128
#define DFP_BINARY_OP		dnn_binary_op
#define DFP_COMPARE_OP		dnn_compare_op
#define decFloat		decQuad
#define DEC_FLOAT_ADD		decQuadAdd
#define DEC_FLOAT_SUBTRACT	decQuadSubtract
#define DEC_FLOAT_MULTIPLY	decQuadMultiply
#define DEC_FLOAT_DIVIDE	decQuadDivide
#define DEC_FLOAT_COMPARE	decQuadCompare
#define DEC_FLOAT_IS_ZERO	decQuadIsZero
#define DEC_FLOAT_IS_NAN	decQuadIsNaN
#define DEC_FLOAT_IS_SIGNED	decQuadIsSigned
#endif

/* Names of functions to convert between different decimal float types.  */

#if WIDTH == 32
#if WIDTH_TO == 64
#define DFP_TO_DFP	DPD_BID_NAME(__dpd_extendsddd2,__bid_extendsddd2)
#elif WIDTH_TO == 128
#define DFP_TO_DFP	DPD_BID_NAME(__dpd_extendsdtd2,__bid_extendsdtd2)
#endif
#elif WIDTH == 64	
#if WIDTH_TO == 32
#define DFP_TO_DFP	DPD_BID_NAME(__dpd_truncddsd2,__bid_truncddsd2)
#elif WIDTH_TO == 128
#define DFP_TO_DFP	DPD_BID_NAME(__dpd_extendddtd2,__bid_extendddtd2)
#endif
#elif WIDTH == 128
#if WIDTH_TO == 32
#define DFP_TO_DFP	DPD_BID_NAME(__dpd_trunctdsd2,__bid_trunctdsd2)
#elif WIDTH_TO == 64
#define DFP_TO_DFP	DPD_BID_NAME(__dpd_trunctddd2,__bid_trunctddd2)
#endif
#endif

/* Names of functions to convert between decimal float and integers.  */

#if WIDTH == 32
#if INT_KIND == 1
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatsisd,__bid_floatsisd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixsdsi,__bid_fixsdsi)
#define DEC_FLOAT_FROM_INT decDoubleFromInt32
#define DEC_FLOAT_TO_INT   decDoubleToInt32
#elif INT_KIND == 2
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatdisd,__bid_floatdisd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixsddi,__bid_fixsddi)
#elif INT_KIND == 3
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatunssisd,__bid_floatunssisd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixunssdsi,__bid_fixunssdsi)
#define DEC_FLOAT_FROM_INT decDoubleFromUInt32
#define DEC_FLOAT_TO_INT   decDoubleToUInt32
#elif INT_KIND == 4
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatunsdisd,__bid_floatunsdisd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixunssddi,__bid_fixunssddi)
#endif
#elif WIDTH == 64
#define decFloat	decDouble
#if INT_KIND == 1
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatsidd,__bid_floatsidd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixddsi,__bid_fixddsi)
#define DEC_FLOAT_FROM_INT decDoubleFromInt32
#define DEC_FLOAT_TO_INT   decDoubleToInt32
#elif INT_KIND == 2
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatdidd,__bid_floatdidd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixdddi,__bid_fixdddi)
#elif INT_KIND == 3
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatunssidd,__bid_floatunssidd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixunsddsi,__bid_fixunsddsi)
#define DEC_FLOAT_FROM_INT decDoubleFromUInt32
#define DEC_FLOAT_TO_INT   decDoubleToUInt32
#elif INT_KIND == 4
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatunsdidd,__bid_floatunsdidd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixunsdddi,__bid_fixunsdddi)
#endif
#elif WIDTH == 128
#define decFloat	decQuad
#if INT_KIND == 1
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatsitd,__bid_floatsitd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixtdsi,__bid_fixtdsi)
#define DEC_FLOAT_FROM_INT decQuadFromInt32
#define DEC_FLOAT_TO_INT   decQuadToInt32
#elif INT_KIND == 2
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatditd,__bid_floatditd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixtddi,__bid_fixtddi)
#elif INT_KIND == 3
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatunssitd,__bid_floatunssitd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixunstdsi,__bid_fixunstdsi)
#define DEC_FLOAT_FROM_INT decQuadFromUInt32
#define DEC_FLOAT_TO_INT   decQuadToUInt32
#elif INT_KIND == 4
#define INT_TO_DFP	DPD_BID_NAME(__dpd_floatunsditd,__bid_floatunsditd)
#define DFP_TO_INT	DPD_BID_NAME(__dpd_fixunstddi,__bid_fixunstddi)
#endif
#endif

/* Names of functions to convert between decimal float and binary float.  */

#if WIDTH == 32
#if BFP_KIND == 1
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extendsfsd,__bid_extendsfsd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_truncsdsf,__bid_truncsdsf)
#elif BFP_KIND == 2
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_truncdfsd,__bid_truncdfsd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_extendsddf,__bid_extendsddf)
#elif BFP_KIND == 3
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_truncxfsd,__bid_truncxfsd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_extendsdxf,__bid_extendsdxf)
#elif BFP_KIND == 4
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_trunctfsd,__bid_trunctfsd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_extendsdtf,__bid_extendsdtf)
#endif /* BFP_KIND */

#elif WIDTH == 64
#if BFP_KIND == 1
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extendsfdd,__bid_extendsfdd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_truncddsf,__bid_truncddsf)
#elif BFP_KIND == 2
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extenddfdd,__bid_extenddfdd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_truncdddf,__bid_truncdddf)
#elif BFP_KIND == 3
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_truncxfdd,__bid_truncxfdd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_extendddxf,__bid_extendddxf)
#elif BFP_KIND == 4
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_trunctfdd,__bid_trunctfdd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_extendddtf,__bid_extendddtf)
#endif /* BFP_KIND */

#elif WIDTH == 128
#if BFP_KIND == 1
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extendsftd,__bid_extendsftd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_trunctdsf,__bid_trunctdsf)
#elif BFP_KIND == 2
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extenddftd,__bid_extenddftd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_trunctddf,__bid_trunctddf)
#elif BFP_KIND == 3
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extendxftd,__bid_extendxftd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_trunctdxf,__bid_trunctdxf)
#elif BFP_KIND == 4
#define BFP_TO_DFP	DPD_BID_NAME(__dpd_extendtftd,__bid_extendtftd)
#define DFP_TO_BFP	DPD_BID_NAME(__dpd_trunctdtf,__bid_trunctdtf)
#endif /* BFP_KIND */

#endif /* WIDTH */

/* Some handy typedefs.  */

typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));
#if LONG_DOUBLE_HAS_XF_MODE
typedef float XFtype __attribute__ ((mode (XF)));
#endif /* LONG_DOUBLE_HAS_XF_MODE */
#if LONG_DOUBLE_HAS_TF_MODE
typedef float TFtype __attribute__ ((mode (TF)));
#endif /* LONG_DOUBLE_HAS_TF_MODE */

typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

/* The type of the result of a decimal float comparison.  This must
   match `__libgcc_cmp_return__' in GCC for the target.  */

typedef int CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));

/* Prototypes.  */

#if defined (L_mul_sd) || defined (L_mul_dd) || defined (L_mul_td)
extern DFP_C_TYPE DFP_MULTIPLY (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_div_sd) || defined (L_div_dd) || defined (L_div_td)
extern DFP_C_TYPE DFP_DIVIDE (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_addsub_sd) || defined (L_addsub_dd) || defined (L_addsub_td)
extern DFP_C_TYPE DFP_ADD (DFP_C_TYPE, DFP_C_TYPE);
extern DFP_C_TYPE DFP_SUB (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_eq_sd) || defined (L_eq_dd) || defined (L_eq_td)
extern CMPtype DFP_EQ (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_ne_sd) || defined (L_ne_dd) || defined (L_ne_td)
extern CMPtype DFP_NE (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_lt_sd) || defined (L_lt_dd) || defined (L_lt_td)
extern CMPtype DFP_LT (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_gt_sd) || defined (L_gt_dd) || defined (L_gt_td)
extern CMPtype DFP_GT (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_le_sd) || defined (L_le_dd) || defined (L_le_td)
extern CMPtype DFP_LE (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_ge_sd) || defined (L_ge_dd) || defined (L_ge_td)
extern CMPtype DFP_GE (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_unord_sd) || defined (L_unord_dd) || defined (L_unord_td)
extern CMPtype DFP_UNORD (DFP_C_TYPE, DFP_C_TYPE);
#endif

#if defined (L_sd_to_dd) || defined (L_sd_to_td) || defined (L_dd_to_sd) \
 || defined (L_dd_to_td) || defined (L_td_to_sd) || defined (L_td_to_dd)
extern DFP_C_TYPE_TO DFP_TO_DFP (DFP_C_TYPE);
#endif

#if defined (L_sd_to_si) || defined (L_dd_to_si) || defined (L_td_to_si) \
 || defined (L_sd_to_di) || defined (L_dd_to_di) || defined (L_td_to_di) \
 || defined (L_sd_to_usi) || defined (L_dd_to_usi) || defined (L_td_to_usi) \
 || defined (L_sd_to_udi) || defined (L_dd_to_udi) || defined (L_td_to_udi)
extern INT_TYPE DFP_TO_INT (DFP_C_TYPE);
#endif

#if defined (L_si_to_sd) || defined (L_si_to_dd) || defined (L_si_to_td) \
 || defined (L_di_to_sd) || defined (L_di_to_dd) || defined (L_di_to_td) \
 || defined (L_usi_to_sd) || defined (L_usi_to_dd) || defined (L_usi_to_td) \
 || defined (L_udi_to_sd) || defined (L_udi_to_dd) || defined (L_udi_to_td)
extern DFP_C_TYPE INT_TO_DFP (INT_TYPE);
#endif

#if defined (L_sd_to_sf) || defined (L_dd_to_sf) || defined (L_td_to_sf) \
 || defined (L_sd_to_df) || defined (L_dd_to_df) || defined (L_td_to_df) \
 || ((defined (L_sd_to_xf) || defined (L_dd_to_xf) || defined (L_td_to_xf)) \
     && LONG_DOUBLE_HAS_XF_MODE) \
 || ((defined (L_sd_to_tf) || defined (L_dd_to_tf) || defined (L_td_to_tf)) \
     && LONG_DOUBLE_HAS_TF_MODE)
extern BFP_TYPE DFP_TO_BFP (DFP_C_TYPE);
#endif

#if defined (L_sf_to_sd) || defined (L_sf_to_dd) || defined (L_sf_to_td) \
 || defined (L_df_to_sd) || defined (L_df_to_dd) || defined (L_df_to_td) \
 || ((defined (L_xf_to_sd) || defined (L_xf_to_dd) || defined (L_xf_to_td)) \
     && LONG_DOUBLE_HAS_XF_MODE) \
 || ((defined (L_tf_to_sd) || defined (L_tf_to_dd) || defined (L_tf_to_td)) \
     && LONG_DOUBLE_HAS_TF_MODE)
extern DFP_C_TYPE BFP_TO_DFP (BFP_TYPE);
#endif

#endif /* _DFPBIT_H */
