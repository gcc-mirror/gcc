/* { dg-do run } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */
/* { dg-require-effective-target riscv_v } */

#define SIZE 128
unsigned short _Alignas (16) in[SIZE];

__attribute__ ((noinline)) int
test (unsigned short sum, unsigned short *in, int x)
{
  for (int j = 0; j < SIZE; j += 8)
    sum += in[j] * x;
  return sum;
}

int
main ()
{
  for (int i = 0; i < SIZE; i++)
    in[i] = i;
  if (test (0, in, 1) != 960)
    __builtin_abort ();
  return 0;
}
