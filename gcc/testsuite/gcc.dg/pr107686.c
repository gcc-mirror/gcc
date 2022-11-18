/* { dg-do compile { target { dfp && longlong64 } } } */
/* { dg-options "-O" } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

typedef _Decimal64 __attribute__((__vector_size__ (64))) D;
typedef __INT32_TYPE__ __attribute__((__vector_size__ (32))) U;
typedef __INT64_TYPE__ __attribute__((__vector_size__ (64))) V;

U u;
D d;

void
foo (void)
{
  d = d < (D) __builtin_convertvector (u, V);
}
