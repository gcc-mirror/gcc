/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O1 -mdejagnu-cpu=power8" } */

typedef __int128_t __attribute__((__vector_size__(16))) vector_128_t;
typedef unsigned long long scalar_64_t;

vector_128_t
foo (void)
{
  union {
    scalar_64_t i64[2];
    vector_128_t v128;
  } u;
  u.i64[0] = 1;
  u.i64[1] = 2;
  return u.v128;
}
