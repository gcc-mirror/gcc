/* Test C2x enumerations with values not representable in int are not diagnosed
   for C11 with -pedantic-errors -Wno-c11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-c11-c2x-compat" } */

enum e1 { e1a = -__LONG_LONG_MAX__ - 1 };

enum e2 { e2a = __LONG_LONG_MAX__ };

enum e3 { e3a = (unsigned int) -1 };

enum e4 { e4a = (long long) -__INT_MAX__ - 1, e4b = (unsigned int) __INT_MAX__ };

enum e5 { e5a = __INT_MAX__, e5b };
