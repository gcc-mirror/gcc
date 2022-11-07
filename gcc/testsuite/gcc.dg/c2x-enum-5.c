/* Test C2x enumerations with values not representable in int.  Test
   -Wc11-c2x-compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

enum e1 { e1a = -__LONG_LONG_MAX__ - 1 }; /* { dg-warning "ISO C restricts enumerator values" } */

enum e2 { e2a = __LONG_LONG_MAX__ }; /* { dg-warning "ISO C restricts enumerator values" } */

enum e3 { e3a = (unsigned int) -1 }; /* { dg-warning "ISO C restricts enumerator values" } */

enum e4 { e4a = (long long) -__INT_MAX__ - 1, e4b = (unsigned int) __INT_MAX__ };
