/* { dg-do assemble { target { int128 && avx512vl } } } */
/* { dg-options "-O2 -mavx512vl" } */

typedef __int128 V __attribute__((vector_size (16)));

void
foo (V *x, V *y, V *z)
{
  register V a __asm ("xmm31") = *z;
  __asm ("" : "+v" (a));
  x[0] = y[0] & a;
  x[1] = y[1] | a;
  x[2] = y[2] ^ a;
}
