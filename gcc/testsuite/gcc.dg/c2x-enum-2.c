/* Test C23 enumerations with values not representable in int.  Test values
   outside the range of standard or extended integer types are diagnosed, even
   when they can be represented in __int128.  */
/* { dg-do compile { target int128 } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

enum e1 { e1a = __LONG_LONG_MAX__, e1b }; /* { dg-error "enumerator value outside the range" } */

enum e2 { e2a = __LONG_LONG_MAX__ * 2ULL + 1ULL, e2b }; /* { dg-error "enumerator value outside the range" } */

/* Likewise, when it's the enum as a whole that can't fit in any standard or
   extended type, but the individual enumerators fit (some fitting a signed
   type and some fitting an unsigned type).  */
enum e3 { e3a = -__LONG_LONG_MAX__ - 1, e3b = __LONG_LONG_MAX__ * 2ULL + 1ULL }; /* { dg-error "enumeration values exceed range of 'intmax_t'" } */
