/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

typedef int X __attribute__((vector_size (32)));

X
foo (int x, int z)
{
  X y = { x, x, x, x, z, z, z, z };
  return y;
}

/* { dg-final { scan-assembler-times "vpbroadcast" "2" } } */
