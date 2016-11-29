/* { dg-do assemble } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O -mavx512f -masm=intel" } */

typedef __int128 V __attribute__((vector_size(64)));

V v;

void
foo()
{
  v ^= (V){1};
}
