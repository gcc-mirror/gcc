/* Header file for fp-bit.c.  */
/* Copyright (C) 2000
   Free Software Foundation, Inc.

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

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef GCC_FP_BIT_H
#define GCC_FP_BIT_H

/* Defining FINE_GRAINED_LIBRARIES allows one to select which routines
   from this file are compiled via additional -D options.

   This avoids the need to pull in the entire fp emulation library
   when only a small number of functions are needed.

   If FINE_GRAINED_LIBRARIES is not defined, then compile every 
   suitable routine.  */
#ifndef FINE_GRAINED_LIBRARIES
#define L_pack_df
#define L_unpack_df
#define L_pack_sf
#define L_unpack_sf
#define L_addsub_sf
#define L_addsub_df
#define L_mul_sf
#define L_mul_df
#define L_div_sf
#define L_div_df
#define L_fpcmp_parts_sf
#define L_fpcmp_parts_df
#define L_compare_sf
#define L_compare_df
#define L_eq_sf
#define L_eq_df
#define L_ne_sf
#define L_ne_df
#define L_gt_sf
#define L_gt_df
#define L_ge_sf
#define L_ge_df
#define L_lt_sf
#define L_lt_df
#define L_le_sf
#define L_le_df
#define L_unord_sf
#define L_unord_df
#define L_usi_to_sf
#define L_usi_to_df
#define L_si_to_sf
#define L_si_to_df
#define L_sf_to_si
#define L_df_to_si
#define L_f_to_usi
#define L_df_to_usi
#define L_negate_sf
#define L_negate_df
#define L_make_sf
#define L_make_df
#define L_sf_to_df
#define L_df_to_sf
#ifdef FLOAT
#define L_thenan_sf
#else
#define L_thenan_df
#endif
#endif /* ! FINE_GRAINED_LIBRARIES */

typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));

typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));

/* The type of the result of a fp compare */
#ifndef CMPtype
#define CMPtype SItype
#endif

typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

#define MAX_USI_INT  (~(USItype)0)
#define MAX_SI_INT   ((SItype) (MAX_USI_INT >> 1))
#define BITS_PER_SI  (4 * BITS_PER_UNIT)

#ifdef FLOAT_ONLY
#define NO_DI_MODE
#endif

#ifdef FLOAT
#	define NGARDS    7L
#	define GARDROUND 0x3f
#	define GARDMASK  0x7f
#	define GARDMSB   0x40
#	define EXPBITS 8
#	define EXPBIAS 127
#	define FRACBITS 23
#	define EXPMAX (0xff)
#	define QUIET_NAN 0x100000L
#	define FRAC_NBITS 32
#	define FRACHIGH  0x80000000L
#	define FRACHIGH2 0xc0000000L
#	define pack_d __pack_f
#	define unpack_d __unpack_f
#	define __fpcmp_parts __fpcmp_parts_f
	typedef USItype fractype;
	typedef UHItype halffractype;
	typedef SFtype FLO_type;
	typedef SItype intfrac;

#else
#	define PREFIXFPDP dp
#	define PREFIXSFDF df
#	define NGARDS 8L
#	define GARDROUND 0x7f
#	define GARDMASK  0xff
#	define GARDMSB   0x80
#	define EXPBITS 11
#	define EXPBIAS 1023
#	define FRACBITS 52
#	define EXPMAX (0x7ff)
#	define QUIET_NAN 0x8000000000000LL
#	define FRAC_NBITS 64
#	define FRACHIGH  0x8000000000000000LL
#	define FRACHIGH2 0xc000000000000000LL
#	define pack_d __pack_d
#	define unpack_d __unpack_d
#	define __fpcmp_parts __fpcmp_parts_d
	typedef UDItype fractype;
	typedef USItype halffractype;
	typedef DFtype FLO_type;
	typedef DItype intfrac;
#endif /* FLOAT */

#ifdef US_SOFTWARE_GOFAST
#	ifdef FLOAT
#		define add 		fpadd
#		define sub 		fpsub
#		define multiply 	fpmul
#		define divide 		fpdiv
#		define compare 		fpcmp
#		define usi_to_float 	__floatunsisf
#		define si_to_float 	sitofp
#		define float_to_si 	fptosi
#		define float_to_usi 	fptoui
#		define negate 		__negsf2
#		define sf_to_df		fptodp
#		define dptofp 		dptofp
#else
#		define add 		dpadd
#		define sub 		dpsub
#		define multiply 	dpmul
#		define divide 		dpdiv
#		define compare 		dpcmp
#		define usi_to_float 	__floatunsidf
#		define si_to_float 	litodp
#		define float_to_si 	dptoli
#		define float_to_usi 	dptoul
#		define negate 		__negdf2
#		define df_to_sf 	dptofp
#	endif /* FLOAT */
#else
#	ifdef FLOAT
#		define add 		__addsf3
#		define sub 		__subsf3
#		define multiply 	__mulsf3
#		define divide 		__divsf3
#		define compare 		__cmpsf2
#		define _eq_f2 		__eqsf2
#		define _ne_f2 		__nesf2
#		define _gt_f2 		__gtsf2
#		define _ge_f2 		__gesf2
#		define _lt_f2 		__ltsf2
#		define _le_f2 		__lesf2
#		define _unord_f2	__unordsf2
#		define usi_to_float 	__floatunsisf
#		define si_to_float 	__floatsisf
#		define float_to_si 	__fixsfsi
#		define float_to_usi 	__fixunssfsi
#		define negate 		__negsf2
#		define sf_to_df		__extendsfdf2
#else
#		define add 		__adddf3
#		define sub 		__subdf3
#		define multiply 	__muldf3
#		define divide 		__divdf3
#		define compare 		__cmpdf2
#		define _eq_f2 		__eqdf2
#		define _ne_f2 		__nedf2
#		define _gt_f2 		__gtdf2
#		define _ge_f2 		__gedf2
#		define _lt_f2 		__ltdf2
#		define _le_f2 		__ledf2
#		define _unord_f2	__unorddf2
#		define usi_to_float 	__floatunsidf
#		define si_to_float 	__floatsidf
#		define float_to_si 	__fixdfsi
#		define float_to_usi 	__fixunsdfsi
#		define negate 		__negdf2
#		define df_to_sf		__truncdfsf2
#	endif /* FLOAT */
#endif /* US_SOFTWARE_GOFAST */

#ifndef INLINE
#define INLINE __inline__
#endif

/* Preserve the sticky-bit when shifting fractions to the right.  */
#define LSHIFT(a) { a = (a & 1) | (a >> 1); }

/* numeric parameters */
/* F_D_BITOFF is the number of bits offset between the MSB of the mantissa
   of a float and of a double. Assumes there are only two float types.
   (double::FRAC_BITS+double::NGARDS-(float::FRAC_BITS-float::NGARDS))
 */
#define F_D_BITOFF (52+8-(23+7))


#define NORMAL_EXPMIN (-(EXPBIAS)+1)
#define IMPLICIT_1 (1LL<<(FRACBITS+NGARDS))
#define IMPLICIT_2 (1LL<<(FRACBITS+1+NGARDS))

/* common types */

typedef enum
{
  CLASS_SNAN,
  CLASS_QNAN,
  CLASS_ZERO,
  CLASS_NUMBER,
  CLASS_INFINITY
} fp_class_type;

typedef struct
{
#ifdef SMALL_MACHINE
  char class;
  unsigned char sign;
  short normal_exp;
#else
  fp_class_type class;
  unsigned int sign;
  int normal_exp;
#endif

  union
    {
      fractype ll;
      halffractype l[2];
    } fraction;
} fp_number_type;

typedef union
{
  FLO_type value;
  fractype value_raw;

#ifndef FLOAT
  halffractype words[2];
#endif

#ifdef FLOAT_BIT_ORDER_MISMATCH
  struct
    {
      fractype fraction:FRACBITS __attribute__ ((packed));
      unsigned int exp:EXPBITS __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
#endif

#ifdef _DEBUG_BITFLOAT
  struct
    {
      unsigned int sign:1 __attribute__ ((packed));
      unsigned int exp:EXPBITS __attribute__ ((packed));
      fractype fraction:FRACBITS __attribute__ ((packed));
    }
  bits_big_endian;

  struct
    {
      fractype fraction:FRACBITS __attribute__ ((packed));
      unsigned int exp:EXPBITS __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits_little_endian;
#endif
}
FLO_union_type;

/* Prototypes */

#if defined(L_pack_df) || defined(L_pack_sf)
extern FLO_type pack_d (fp_number_type *);
#endif

extern void unpack_d (FLO_union_type *, fp_number_type *);

#if defined(L_addsub_sf) || defined(L_addsub_df)
extern FLO_type add (FLO_type, FLO_type);
extern FLO_type sub (FLO_type, FLO_type);
#endif

#if defined(L_mul_sf) || defined(L_mul_df)
extern FLO_type multiply (FLO_type, FLO_type);
#endif

#if defined(L_div_sf) || defined(L_div_df)
extern FLO_type divide (FLO_type, FLO_type);
#endif

extern int __fpcmp_parts (fp_number_type *, fp_number_type *);

#if defined(L_compare_sf) || defined(L_compare_df)
extern CMPtype compare (FLO_type, FLO_type);
#endif

#ifndef US_SOFTWARE_GOFAST

#if defined(L_eq_sf) || defined(L_eq_df)
extern CMPtype _eq_f2 (FLO_type, FLO_type);
#endif

#if defined(L_ne_sf) || defined(L_ne_df)
extern CMPtype _ne_f2 (FLO_type, FLO_type);
#endif

#if defined(L_gt_sf) || defined(L_gt_df)
extern CMPtype _gt_f2 (FLO_type, FLO_type);
#endif

#if defined(L_ge_sf) || defined(L_ge_df)
extern CMPtype _ge_f2 (FLO_type, FLO_type);
#endif

#if defined(L_lt_sf) || defined(L_lt_df)
extern CMPtype _lt_f2 (FLO_type, FLO_type);
#endif

#if defined(L_le_sf) || defined(L_le_df)
extern CMPtype _le_f2 (FLO_type, FLO_type);
#endif

#if defined(L_unord_sf) || defined(L_unord_df)
extern CMPtype _unord_f2 (FLO_type, FLO_type);
#endif

#endif /* ! US_SOFTWARE_GOFAST */

#if defined(L_si_to_sf) || defined(L_si_to_df)
extern FLO_type si_to_float (SItype);
#endif

#if defined(L_sf_to_si) || defined(L_df_to_si)
extern SItype float_to_si (FLO_type);
#endif

#if defined(L_sf_to_usi) || defined(L_df_to_usi)
#ifdef US_SOFTWARE_GOFAST
extern USItype float_to_usi (FLO_type);
#endif
#endif

#if defined(L_usi_to_sf) || defined(L_usi_to_df)
extern FLO_type usi_to_float (USItype);
#endif

#if defined(L_negate_sf) || defined(L_negate_df)
extern FLO_type negate (FLO_type);
#endif

#ifdef FLOAT
#if defined(L_make_sf)
extern SFtype __make_fp (fp_class_type, unsigned int, int, USItype);
#endif
#ifndef FLOAT_ONLY
extern DFtype __make_dp (fp_class_type, unsigned int, int, UDItype);
#if defined(L_sf_to_df)
extern DFtype sf_to_df (SFtype);
#endif
#endif /* ! FLOAT_ONLY */
#endif /* FLOAT */

#ifndef FLOAT
extern SFtype __make_fp (fp_class_type, unsigned int, int, USItype);
#if defined(L_make_df)
extern DFtype __make_dp (fp_class_type, unsigned int, int, UDItype);
#endif
#if defined(L_df_to_sf)
extern SFtype df_to_sf (DFtype);
#endif
#endif /* ! FLOAT */

#endif /* ! GCC_FP_BIT_H */
