/* { dg-options "-mdejagnu-cpu=power9 -ffinite-math-only" } */

int
h9 (__attribute__ ((altivec (vector__))) char un)
{
  return (__builtin_vec_bcdinvalid (un));
}
