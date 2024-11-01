/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O0 -mcmpccxadd" } */
#include <x86gprintrin.h>

short *a;
int b, c;
int *d;
long long e, f;

void extern
cmpccxadd_test(void)
{
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_O); /* { dg-error "incompatible pointer type" } */
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_O); /* { dg-error "incompatible pointer type" } */
}
