/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O3 -msve-vector-bits=256 --param vect-partial-vector-usage=1" } */

#define N 0x1100

#include "reduc_13.c"

int
main (void)
{
  unsigned int x[N];
  for (int i = 0; i < N; ++i)
    x[i] = ((i + 1) * (i + 2)) & 0xfffff;

  unsigned int add_res[2] = { 42, 1111 };
  add_loop (x, add_res);
  if (add_res[0] != 968538154
      || add_res[1] != 964340823)
    __builtin_abort ();

  unsigned int max_res1[2] = { 0, 0 };
  max_loop (x, max_res1);
  if (max_res1[0] != 1048150
      || max_res1[1] != 1045506)
    __builtin_abort ();

  unsigned int max_res2[2] = { 1048151, 1045507 };
  max_loop (x, max_res2);
  if (max_res2[0] != 1048151
      || max_res2[1] != 1045507)
    __builtin_abort ();

  unsigned int or_res[2] = { 0x1000000, 0x2000000 };
  or_loop (x, or_res);
  if (or_res[0] != 0x10ffffe
      || or_res[1] != 0x20ffffe)
    __builtin_abort ();

  unsigned int eor_res[2] = { 0x1000000, 0x2000000 };
  eor_loop (x, eor_res);
  if (eor_res[0] != 0x1010000
      || eor_res[1] != 0x20b5000)
    __builtin_abort ();

  for (int i = 0; i < N; ++i)
    x[i] = ~x[i] & 0xfffff;

  unsigned int min_res1[2] = { 500, 4000 };
  min_loop (x, min_res1);
  if (min_res1[0] != 425
      || min_res1[1] != 3069)
    __builtin_abort ();

  unsigned int min_res2[2] = { 424, 3068 };
  min_loop (x, min_res2);
  if (min_res2[0] != 424
      || min_res2[1] != 3068)
    __builtin_abort ();

  return 0;
}
