/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.5.7 - Bitwise shift operands.
   C99 6.5.3 Unary operators.
   C99 6.5.5 Multiplicative operators.
   C99 6.5.6 Additive operators.
   C99 6.5.7 Bitwise shift operators.
   C99 6.5.8 Relational operators.
   C99 6.5.9 Equality operators.
   C99 6.5.16 Assignment operators.

   Check if all operations on constants are ok.  */

#define ALLOPCONST(TYPE,NAME,POSTFIX) \
	TYPE NAME ## plus = +0.1 ## POSTFIX; \
	unsigned TYPE NAME ## u_plus = +0.1u ## POSTFIX; \
	_Sat TYPE NAME ## sat_plus = +(_Sat TYPE)0.1 ## POSTFIX; \
	_Sat unsigned TYPE NAME ## sat_u_plus = +(_Sat unsigned TYPE)0.1u ## POSTFIX; \
	TYPE NAME ## neg = -0.1 ## POSTFIX; \
	_Sat TYPE NAME ## sat_neg = -(_Sat TYPE)0.1 ## POSTFIX; \
	_Sat unsigned TYPE NAME ## sat_u_neg = -(_Sat unsigned TYPE)0.1u ## POSTFIX; \
	int NAME ## logneg1 = !0.1 ## POSTFIX; \
	int NAME ## logneg1u = !0.1u ## POSTFIX; \
	int NAME ## logneg2 = !0.0 ## POSTFIX; \
	int NAME ## logneg2u = !0.0u ## POSTFIX; \
	TYPE NAME ## add = 0.1 ## POSTFIX + 0.2 ## POSTFIX; \
	unsigned TYPE NAME ## u_add = 0.1u ## POSTFIX + 0.2u ## POSTFIX; \
	_Sat TYPE NAME ## sat_add = (_Sat TYPE)0.8 ## POSTFIX + 0.3 ## POSTFIX; \
	_Sat unsigned TYPE NAME ## sat_u_add = (_Sat unsigned TYPE)0.8u ## POSTFIX + 0.3u ## POSTFIX; \
	TYPE NAME ## sub = 0.1 ## POSTFIX - 0.2 ## POSTFIX; \
	unsigned TYPE NAME ## u_sub = 0.2u ## POSTFIX - 0.1u ## POSTFIX; \
	_Sat TYPE NAME ## sat_sub = (_Sat TYPE)0.1 ## POSTFIX - 0.2 ## POSTFIX; \
	_Sat unsigned TYPE NAME ## sat_u_sub = (_Sat unsigned TYPE)0.1u ## POSTFIX - 0.2u ## POSTFIX; \
	TYPE NAME ## mul = 0.1 ## POSTFIX * 0.2 ## POSTFIX; \
	unsigned TYPE NAME ## u_mul = 0.1u ## POSTFIX * 0.2u ## POSTFIX; \
	_Sat TYPE NAME ## sat_mul = (_Sat TYPE)0.1 ## POSTFIX * 0.2 ## POSTFIX; \
	_Sat unsigned TYPE NAME ## sat_u_mul = (_Sat unsigned TYPE)0.1u ## POSTFIX * 0.2u ## POSTFIX; \
	TYPE NAME ## div = 0.1 ## POSTFIX / 0.3 ## POSTFIX; \
	unsigned TYPE NAME ## u_div = 0.1u ## POSTFIX / 0.3u ## POSTFIX; \
	_Sat TYPE NAME ## sat_div = (_Sat TYPE)0.3 ## POSTFIX / 0.1 ## POSTFIX; \
	_Sat unsigned TYPE NAME ## sat_u_div = (_Sat unsigned TYPE)0.3u ## POSTFIX / 0.1u ## POSTFIX; \
	TYPE NAME ## shl = 0.1 ## POSTFIX << 3; \
	unsigned TYPE NAME ## u_shl = 0.1u ## POSTFIX << 3; \
	_Sat TYPE NAME ## sat_shl = (_Sat TYPE)0.8 ## POSTFIX << 3; \
	_Sat unsigned TYPE NAME ## sat_u_shl = (_Sat unsigned TYPE)0.8u ## POSTFIX << 3; \
	TYPE NAME ## shr = 0.1 ## POSTFIX >> 5; \
	unsigned TYPE NAME ## u_shr = 0.1u ## POSTFIX >> 5; \
	_Sat TYPE NAME ## sat_shr = (_Sat TYPE)0.1 ## POSTFIX >> 5; \
	_Sat unsigned TYPE NAME ## sat_u_shr = (_Sat unsigned TYPE)0.1 ## POSTFIX >> 5; \
	int NAME ## ne = 0.1 ## POSTFIX != 0.2 ## POSTFIX; \
	int NAME ## u_ne = 0.1u ## POSTFIX != 0.2u ## POSTFIX; \
	int NAME ## eq = 0.1 ## POSTFIX == 0.2 ## POSTFIX; \
	int NAME ## u_eq = 0.1u ## POSTFIX == 0.2u ## POSTFIX; \
	int NAME ## gt = 0.1 ## POSTFIX > 0.2 ## POSTFIX; \
	int NAME ## u_gt = 0.1u ## POSTFIX > 0.2u ## POSTFIX; \
	int NAME ## ge = 0.1 ## POSTFIX >= 0.2 ## POSTFIX; \
	int NAME ## u_ge = 0.1u ## POSTFIX >= 0.2 ## POSTFIX; \
	int NAME ## lt = 0.1 ## POSTFIX < 0.2 ## POSTFIX; \
	int NAME ## u_lt = 0.1u ## POSTFIX < 0.2 ## POSTFIX; \
	int NAME ## le = 0.1 ## POSTFIX <= 0.2 ## POSTFIX; \
	int NAME ## u_le = 0.1u ## POSTFIX <= 0.2 ## POSTFIX; \
	int NAME ## ne2 = 0.2 ## POSTFIX != 0.2 ## POSTFIX; \
	int NAME ## u_ne2 = 0.2u ## POSTFIX != 0.2u ## POSTFIX; \
	int NAME ## eq2 = 0.2 ## POSTFIX == 0.2 ## POSTFIX; \
	int NAME ## u_eq2 = 0.2u ## POSTFIX == 0.2u ## POSTFIX; \
	int NAME ## gt2 = 0.2 ## POSTFIX > 0.2 ## POSTFIX; \
	int NAME ## u_gt2 = 0.2u ## POSTFIX > 0.2u ## POSTFIX; \
	int NAME ## ge2 = 0.2 ## POSTFIX >= 0.2 ## POSTFIX; \
	int NAME ## u_ge2 = 0.2u ## POSTFIX >= 0.2 ## POSTFIX; \
	int NAME ## lt2 = 0.2 ## POSTFIX < 0.2 ## POSTFIX; \
	int NAME ## u_lt2 = 0.2u ## POSTFIX < 0.2 ## POSTFIX; \
	int NAME ## le2 = 0.2 ## POSTFIX <= 0.2 ## POSTFIX; \
	int NAME ## u_le2 = 0.2u ## POSTFIX <= 0.2 ## POSTFIX; \
	TYPE NAME ## hr_signed = 0.1hr; \
	unsigned TYPE NAME ## hr_unsigned = 0.1hr; \
	_Sat TYPE NAME ## hr_sat_signed = 0.1hr; \
	_Sat unsigned TYPE NAME ## hr_sat_unsigned = 0.1hr; \
	TYPE NAME ## r_signed = 0.1r; \
	unsigned TYPE NAME ## r_unsigned = 0.1r; \
	_Sat TYPE NAME ## r_sat_signed = 0.1r; \
	_Sat unsigned TYPE NAME ## r_sat_unsigned = 0.1r; \
	TYPE NAME ## lr_signed = 0.1lr; \
	unsigned TYPE NAME ## lr_unsigned = 0.1lr; \
	_Sat TYPE NAME ## lr_sat_signed = 0.1lr; \
	_Sat unsigned TYPE NAME ## lr_sat_unsigned = 0.1lr; \
	TYPE NAME ## llr_signed = 0.1llr; \
	unsigned TYPE NAME ## llr_unsigned = 0.1llr; \
	_Sat TYPE NAME ## llr_sat_signed = 0.1llr; \
	_Sat unsigned TYPE NAME ## llr_sat_unsigned = 0.1llr; \
	TYPE NAME ## uhr_signed = 0.1uhr; \
	unsigned TYPE NAME ## uhr_unsigned = 0.1uhr; \
	_Sat TYPE NAME ## uhr_sat_signed = 0.1uhr; \
	_Sat unsigned TYPE NAME ## uhr_sat_unsigned = 0.1uhr; \
	TYPE NAME ## ur_signed = 0.1ur; \
	unsigned TYPE NAME ## ur_unsigned = 0.1ur; \
	_Sat TYPE NAME ## ur_sat_signed = 0.1ur; \
	_Sat unsigned TYPE NAME ## ur_sat_unsigned = 0.1ur; \
	TYPE NAME ## ulr_signed = 0.1ulr; \
	unsigned TYPE NAME ## ulr_unsigned = 0.1ulr; \
	_Sat TYPE NAME ## ulr_sat_signed = 0.1ulr; \
	_Sat unsigned TYPE NAME ## ulr_sat_unsigned = 0.1ulr; \
	TYPE NAME ## ullr_signed = 0.1ullr; \
	unsigned TYPE NAME ## ullr_unsigned = 0.1ullr; \
	_Sat TYPE NAME ## ullr_sat_signed = 0.1ullr; \
	_Sat unsigned TYPE NAME ## ullr_sat_unsigned = 0.1ullr; \
	TYPE NAME ## hk_signed = 0.1hk; \
	unsigned TYPE NAME ## hk_unsigned = 0.1hk; \
	_Sat TYPE NAME ## hk_sat_signed = 5.1hk; \
	_Sat unsigned TYPE NAME ## hk_sat_unsigned = 5.1hk; \
	TYPE NAME ## k_signed = 0.1k; \
	unsigned TYPE NAME ## k_unsigned = 0.1k; \
	_Sat TYPE NAME ## k_sat_signed = 5.1k; \
	_Sat unsigned TYPE NAME ## k_sat_unsigned = 5.1k; \
	TYPE NAME ## lk_signed = 0.1lk; \
	unsigned TYPE NAME ## lk_unsigned = 0.1lk; \
	_Sat TYPE NAME ## lk_sat_signed = 5.1lk; \
	_Sat unsigned TYPE NAME ## lk_sat_unsigned = 5.1lk; \
	TYPE NAME ## llk_signed = 0.1llk; \
	unsigned TYPE NAME ## llk_unsigned = 0.1llk; \
	_Sat TYPE NAME ## llk_sat_signed = 5.1llk; \
	_Sat unsigned TYPE NAME ## llk_sat_unsigned = 5.1llk; \
	TYPE NAME ## uhk_signed = 0.1uhk; \
	unsigned TYPE NAME ## uhk_unsigned = 0.1uhk; \
	_Sat TYPE NAME ## uhk_sat_signed = 5.1uhk; \
	_Sat unsigned TYPE NAME ## uhk_sat_unsigned = 5.1uhk; \
	TYPE NAME ## uk_signed = 0.1uk; \
	unsigned TYPE NAME ## uk_unsigned = 0.1uk; \
	_Sat TYPE NAME ## uk_sat_signed = 5.1uk; \
	_Sat unsigned TYPE NAME ## uk_sat_unsigned = 5.1uk; \
	TYPE NAME ## ulk_signed = 0.1ulk; \
	unsigned TYPE NAME ## ulk_unsigned = 0.1ulk; \
	_Sat TYPE NAME ## ulk_sat_signed = 5.1ulk; \
	_Sat unsigned TYPE NAME ## ulk_sat_unsigned = 5.1ulk; \
	TYPE NAME ## ullk_signed = 0.1ullk; \
	unsigned TYPE NAME ## ullk_unsigned = 0.1ullk; \
	_Sat TYPE NAME ## ullk_sat_signed = 5.1ullk; \
	_Sat unsigned TYPE NAME ## ullk_sat_unsigned = 5.1ullk; \

ALLOPCONST(short _Fract, sf, hr);
ALLOPCONST(_Fract, f, r);
ALLOPCONST(long _Fract, lf, lr);
ALLOPCONST(long long _Fract, llf, llr);
ALLOPCONST(short _Accum, sa, hk);
ALLOPCONST(_Accum, a, k);
ALLOPCONST(long _Accum, la, lk);
ALLOPCONST(long long _Accum, lla, llk);
