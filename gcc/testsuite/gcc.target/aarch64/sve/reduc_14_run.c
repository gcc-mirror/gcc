/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O3 -msve-vector-bits=256 --param vect-partial-vector-usage=1" } */

#define N 0x1100

#include "reduc_14.c"

int
main (void)
{
  unsigned int x[N];
  for (int i = 0; i < N; ++i)
    x[i] = ((i + 1) * (i + 2)) & 0xfffff;

  unsigned int add_res1[2] = { 11, 22 };
  add_loop (x, 0, add_res1);
  if (add_res1[0] != 11
      || add_res1[1] != 22)
    __builtin_abort ();

  unsigned int add_res2[2] = { 10, 20 };
  add_loop (x, 11, add_res2);
  if (add_res2[0] != 1902
      || add_res2[1] != 2176)
    __builtin_abort ();

  unsigned int add_res3[2] = { 15, 30 };
  add_loop (x, 0x100, add_res3);
  if (add_res3[0] != 22435087
      || add_res3[1] != 22566686)
    __builtin_abort ();

  unsigned int add_res4[2] = { 100, 200 };
  add_loop (x, 0x11f, add_res4);
  if (add_res4[0] != 31602244
      || add_res4[1] != 31767656)
    __builtin_abort ();

  unsigned int max_res1[2] = { 461, 500 };
  max_loop (x, 11, max_res1);
  if (max_res1[0] != 462
      || max_res1[1] != 506)
    __builtin_abort ();

  unsigned int max_res2[2] = { 463, 507 };
  max_loop (x, 11, max_res2);
  if (max_res2[0] != 463
      || max_res2[1] != 507)
    __builtin_abort ();

  unsigned int max_res3[2] = { 1000000, 1000000 };
  max_loop (x, 0x200, max_res3);
  if (max_res3[0] != 1047552
      || max_res3[1] != 1045506)
    __builtin_abort ();

  unsigned int max_res4[2] = { 1047553, 1045507 };
  max_loop (x, 0x200, max_res4);
  if (max_res4[0] != 1047553
      || max_res4[1] != 1045507)
    __builtin_abort ();

  unsigned int max_res5[2] = { 300000, 30000 };
  max_loop (x, 0x11f, max_res5);
  if (max_res5[0] != 328902
      || max_res5[1] != 330050)
    __builtin_abort ();

  unsigned int max_res6[2] = { 328903, 330051 };
  max_loop (x, 0x11f, max_res6);
  if (max_res6[0] != 328903
      || max_res6[1] != 330051)
    __builtin_abort ();

  unsigned int or_res1[2] = { 11, 22 };
  or_loop (x, 0, or_res1);
  if (or_res1[0] != 11
      || or_res1[1] != 22)
    __builtin_abort ();

  unsigned int or_res2[2] = { 0x200000, 0xe00000 };
  or_loop (x, 11, or_res2);
  if (or_res2[0] != 0x2001fe
      || or_res2[1] != 0xe001fe)
    __builtin_abort ();

  unsigned int or_res3[2] = { 0x800000, 0x700000 };
  or_loop (x, 0x40, or_res3);
  if (or_res3[0] != 0x803ffe
      || or_res3[1] != 0x707ffe)
    __builtin_abort ();

  unsigned int or_res4[2] = { 0x100001, 0x300000 };
  or_loop (x, 0x4f, or_res4);
  if (or_res4[0] != 0x107fff
      || or_res4[1] != 0x307ffe)
    __builtin_abort ();

  unsigned int eor_res1[2] = { 11, 22 };
  eor_loop (x, 0, eor_res1);
  if (eor_res1[0] != 11
      || eor_res1[1] != 22)
    __builtin_abort ();

  unsigned int eor_res2[2] = { 0x2000ff, 0xe000ff };
  eor_loop (x, 11, eor_res2);
  if (eor_res2[0] != 0x2001cf
      || eor_res2[1] != 0xe000b7)
    __builtin_abort ();

  unsigned int eor_res3[2] = { 0x805000, 0x70f000 };
  eor_loop (x, 0x100, eor_res3);
  if (eor_res3[0] != 0x824200
      || eor_res3[1] != 0x77dc00)
    __builtin_abort ();

  unsigned int eor_res4[2] = { 0x101201, 0x300f00 };
  eor_loop (x, 0x11f, eor_res4);
  if (eor_res4[0] != 0x178801
      || eor_res4[1] != 0x337240)
    __builtin_abort ();

  for (int i = 0; i < N; ++i)
    x[i] = ~x[i] & 0xfffff;

  unsigned int min_res1[2] = { 1048200, 1048100 };
  min_loop (x, 11, min_res1);
  if (min_res1[0] != 1048113
      || min_res1[1] != 1048069)
    __builtin_abort ();

  unsigned int min_res2[2] = { 1048112, 1048068 };
  min_loop (x, 11, min_res2);
  if (min_res2[0] != 1048112
      || min_res2[1] != 1048068)
    __builtin_abort ();

  unsigned int min_res3[2] = { 10000, 10000 };
  min_loop (x, 0x200, min_res3);
  if (min_res3[0] != 1023
      || min_res3[1] != 3069)
    __builtin_abort ();

  unsigned int min_res4[2] = { 1022, 3068 };
  min_loop (x, 0x200, min_res4);
  if (min_res4[0] != 1022
      || min_res4[1] != 3068)
    __builtin_abort ();

  unsigned int min_res5[2] = { 719680, 718530 };
  min_loop (x, 0x11f, min_res5);
  if (min_res5[0] != 719673
      || min_res5[1] != 718525)
    __builtin_abort ();

  unsigned int min_res6[2] = { 719672, 718524 };
  min_loop (x, 0x11f, min_res6);
  if (min_res6[0] != 719672
      || min_res6[1] != 718524)
    __builtin_abort ();

  unsigned int and_res1[2] = { 11, 22 };
  and_loop (x, 0, and_res1);
  if (and_res1[0] != 11
      || and_res1[1] != 22)
    __builtin_abort ();

  unsigned int and_res2[2] = { 0xf5cff, 0xf78ff };
  and_loop (x, 11, and_res2);
  if (and_res2[0] != 0xf5c01
      || and_res2[1] != 0xf7801)
    __builtin_abort ();

  unsigned int and_res3[2] = { 0x7efff, 0xecfff };
  and_loop (x, 0x40, and_res3);
  if (and_res3[0] != 0x7c001
      || and_res3[1] != 0xe8001)
    __builtin_abort ();

  unsigned int and_res4[2] = { 0xffffff, 0xffffff };
  and_loop (x, 0x4f, and_res4);
  if (and_res4[0] != 0xf8001
      || and_res4[1] != 0xf8001)
    __builtin_abort ();

  return 0;
}
