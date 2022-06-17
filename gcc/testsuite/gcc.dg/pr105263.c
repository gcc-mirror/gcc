/* { dg-do compile { target dfp } } */
/* { dg-options "-O -ffast-math -w -Wno-psabi" } */

typedef _Decimal64 __attribute__((__vector_size__ (8))) U;
typedef _Decimal64 __attribute__((__vector_size__ (16))) V;

V v;

U
foo (U u)
{
  u *= u;
  u *= -(U){ v[1] };
  return u;
}
