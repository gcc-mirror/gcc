/* float.h for target with VAX floating point formats */
#ifndef _FLOAT_H_
#define _FLOAT_H_
/* Produced by enquire version 4.3, CWI, Amsterdam */

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
#define FLT_MIN_EXP (-127)
   /* Minimum normalised float */
#undef FLT_MIN
#define FLT_MIN 2.93873588e-39F
   /* Minimum int x such that 10**x is a normalised float */
#undef FLT_MIN_10_EXP
#define FLT_MIN_10_EXP (-38)
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable float */
#undef FLT_MAX_EXP
#define FLT_MAX_EXP 127
   /* Maximum float */
#undef FLT_MAX
#define FLT_MAX 1.70141173e+38F
   /* Maximum int x such that 10**x is a representable float */
#undef FLT_MAX_10_EXP
#define FLT_MAX_10_EXP 38

   /* Number of base-FLT_RADIX digits in the significand of a double */
#undef DBL_MANT_DIG
#define DBL_MANT_DIG 56
   /* Number of decimal digits of precision in a double */
#undef DBL_DIG
#define DBL_DIG 16
   /* Difference between 1.0 and the minimum double greater than 1.0 */
#undef DBL_EPSILON
#define DBL_EPSILON 2.77555756156289135e-17
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised double */
#undef DBL_MIN_EXP
#define DBL_MIN_EXP (-127)
   /* Minimum normalised double */
#undef DBL_MIN
#define DBL_MIN 2.93873587705571877e-39
   /* Minimum int x such that 10**x is a normalised double */
#undef DBL_MIN_10_EXP
#define DBL_MIN_10_EXP (-38)
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable double */
#undef DBL_MAX_EXP
#define DBL_MAX_EXP 127
   /* Maximum double */
#undef DBL_MAX
#define DBL_MAX 1.70141183460469229e+38
   /* Maximum int x such that 10**x is a representable double */
#undef DBL_MAX_10_EXP
#define DBL_MAX_10_EXP 38

   /* Number of base-FLT_RADIX digits in the significand of a long double */
#undef LDBL_MANT_DIG
#define LDBL_MANT_DIG 56
   /* Number of decimal digits of precision in a long double */
#undef LDBL_DIG
#define LDBL_DIG 16
   /* Difference between 1.0 and the minimum long double greater than 1.0 */
#undef LDBL_EPSILON
#define LDBL_EPSILON 2.77555756156289135e-17
   /* Minimum int x such that FLT_RADIX**(x-1) is a normalised long double */
#undef LDBL_MIN_EXP
#define LDBL_MIN_EXP (-127)
   /* Minimum normalised long double */
#undef LDBL_MIN
#define LDBL_MIN 2.93873587705571877e-39
   /* Minimum int x such that 10**x is a normalised long double */
#undef LDBL_MIN_10_EXP
#define LDBL_MIN_10_EXP (-38)
   /* Maximum int x such that FLT_RADIX**(x-1) is a representable long double */
#undef LDBL_MAX_EXP
#define LDBL_MAX_EXP 127
   /* Maximum long double */
#undef LDBL_MAX
#define LDBL_MAX 1.70141183460469229e+38
   /* Maximum int x such that 10**x is a representable long double */
#undef LDBL_MAX_10_EXP
#define LDBL_MAX_10_EXP 38

#endif /*  _FLOAT_H_ */
