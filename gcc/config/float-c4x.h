/* float.h for target with TMS320C3x/C4x floating point format  */
#ifndef _FLOAT_H_
#define _FLOAT_H_
/* Produced by enquire version 4.3, CWI, Amsterdam     */

   /* Radix of exponent representation */
#undef FLT_RADIX
#define FLT_RADIX 2 
   /* Number of base-FLT_RADIX digits in the significand of a float */
#undef FLT_MANT_DIG
#define FLT_MANT_DIG 24 
   /* Number of decimal digits of precision in a float */
#undef FLT_DIG
#define FLT_DIG 6 
   /* Addition rounds to 0: zero, 1: nearest, 2: +inf, 3: -inf, -1: unknown */
#undef FLT_ROUNDS
#define FLT_ROUNDS 1  
   /* Difference between 1.0 and the minimum float greater than 1.0 */
#undef FLT_EPSILON
#define FLT_EPSILON 1.19209290e-07F 
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised float */
#undef FLT_MIN_EXP
#define FLT_MIN_EXP (-126) 
   /* Minimum normalised float */
#undef FLT_MIN
#define FLT_MIN 5.8774718E-39F 
   /* Minimum int x such that 10**x is a normalised float */
#undef FLT_MIN_10_EXP
#define FLT_MIN_10_EXP (-39) 
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable float */
#undef FLT_MAX_EXP
#define FLT_MAX_EXP 128 
   /* Maximum float */
#undef FLT_MAX
#define FLT_MAX  3.4028235e+38F 
   /* Maximum int x such that 10**x is a representable float */
#undef FLT_MAX_10_EXP
#define FLT_MAX_10_EXP 38

   /* Number of base-FLT_RADIX digits in the significand of a double */
#undef DBL_MANT_DIG
#define DBL_MANT_DIG 24
   /* Number of decimal digits of precision in a double */
#undef DBL_DIG
#define DBL_DIG 6
   /* Difference between 1.0 and the minimum double greater than 1.0 */
#undef DBL_EPSILON
#define DBL_EPSILON 1.1920929e-07
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised double */
#undef DBL_MIN_EXP
#define DBL_MIN_EXP (-126)
   /* Minimum normalised double */
#undef DBL_MIN
#define DBL_MIN 5.8774718E-39
   /* Minimum int x such that 10**x is a normalised double */
#undef DBL_MIN_10_EXP
#define DBL_MIN_10_EXP -39
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable double */
#undef DBL_MAX_EXP
#define DBL_MAX_EXP 128
   /* Maximum double */
#undef DBL_MAX
#define DBL_MAX 3.4028235E+38
   /* Maximum int x such that 10**x is a representable double */
#undef DBL_MAX_10_EXP
#define DBL_MAX_10_EXP 38

   /* Number of base-FLT_RADIX digits in the significand of a long double */
#undef LDBL_MANT_DIG
#define LDBL_MANT_DIG 32
   /* Number of decimal digits of precision in a long double */
#undef LDBL_DIG
#define LDBL_DIG 8
   /* Difference between 1.0 and the minimum long double greater than 1.0 */
#undef LDBL_EPSILON
#define LDBL_EPSILON 1.19209287e-07L
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised long double */
#undef LDBL_MIN_EXP
#define LDBL_MIN_EXP (-126)
   /* Minimum normalised long double */
#undef LDBL_MIN
#define LDBL_MIN 5.8774717535e-39L
   /* Minimum int x such that 10**x is a normalised long double */
#undef LDBL_MIN_10_EXP
#define LDBL_MIN_10_EXP (-39)
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable long double */
#undef LDBL_MAX_EXP
#define LDBL_MAX_EXP 128
   /* Maximum long double */
#undef LDBL_MAX
#define LDBL_MAX .4028236688e+38L
   /* Maximum int x such that 10**x is a representable long double */
#undef LDBL_MAX_10_EXP
#define LDBL_MAX_10_EXP 38

#endif /*  _FLOAT_H_ */
