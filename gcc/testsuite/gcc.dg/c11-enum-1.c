/* Test C23 enumerations with values not representable in int are diagnosed for
   C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

enum e1 { e1a = -__LONG_LONG_MAX__ - 1 }; /* { dg-error "ISO C restricts enumerator values" } */

enum e2 { e2a = __LONG_LONG_MAX__ }; /* { dg-error "ISO C restricts enumerator values" } */

enum e3 { e3a = (unsigned int) -1 }; /* { dg-error "ISO C restricts enumerator values" } */

enum e4 { e4a = (long long) -__INT_MAX__ - 1, e4b = (unsigned int) __INT_MAX__ };

enum e5 { e5a = __INT_MAX__, e5b }; /* { dg-error "ISO C restricts enumerator values" } */
