/* PR target/85281 */
/* { dg-do assemble { target avx512bw } } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O -mavx512bw -masm=intel -w" } */

typedef char V __attribute__ ((__vector_size__ (64)));

V
foo (V v)
{
  v[8] /= (unsigned __int128) 0;
  v[0] -= ~255;
  return v;
}
