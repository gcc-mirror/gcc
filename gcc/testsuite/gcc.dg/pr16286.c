/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */

/* PR 16286
 * Compilation of a simple Altivec test program fails if vector is #undefined
 * when compiling with -maltivec.  This may be done for building C++ programs
 * that use the STL <vector>.
 */

#include <altivec.h>
#undef vector

void test(void)
{
        __vector unsigned int a, b;
        a = vec_and(a, b);
}

/* { dg-bogus "syntax error before \"vector\"" "-maltivec" { target powerpc*-*-* } 0 } */
