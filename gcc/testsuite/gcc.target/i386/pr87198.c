/* PR target/87198 */
/* { dg-do compile } */
/* { dg-options "-O2 -mxsavec -mno-xsave" } */

#include <x86intrin.h>

void
test_xsavec (void *__A, long long __B)
{
  _xsavec (__A, __B);
}

/* { dg-error "target specific option mismatch" "" { target *-*-* } 0 } */
