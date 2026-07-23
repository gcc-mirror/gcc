/* PR tree-optimization/112104 */
/* { dg-do run } */
/* { dg-options "-O1" } */

#include "pr112104-1.c"

int
main (void)
{
  unsigned int tmp = 0x1101;
  unsigned int inv = 0x111101;
  unsigned long long tmp64 = 0x1101ULL;
  unsigned long long inv64 = 0xdeadbeefcafebabeULL;

  if (f_xor (tmp, 64, inv) != 0x1101)
    __builtin_abort ();
  if (f_xor (tmp, 63, inv) != 0x110000)
    __builtin_abort ();

  if (f_xor (tmp, 1, inv) != 0x110000)
    __builtin_abort ();
  if (f_xor (tmp, 2, inv) != 0x1101)
    __builtin_abort ();

  if (f_xor1 (64) != 0)
    __builtin_abort ();
  if (f_xor1 (63) != 1)
    __builtin_abort ();
  if (f_xor1 (1) != 1)
    __builtin_abort ();
  if (f_xor1 (2) != 0)
    __builtin_abort ();

  if (f_xor64 (tmp64, 64, inv64) != 0x1101ULL)
    __builtin_abort ();
  if (f_xor64 (tmp64, 63, inv64) != 0xdeadbeefcafeabbfULL)
    __builtin_abort ();
  if (f_xor64 (tmp64, 1, inv64) != 0xdeadbeefcafeabbfULL)
    __builtin_abort ();
  if (f_xor64 (tmp64, 2, inv64) != 0x1101ULL)
    __builtin_abort ();

  if (f_xorc (tmp, 64) != 0x1101)
    __builtin_abort ();
  if (f_xorc (tmp, 63) != 0x3d29)
    __builtin_abort ();
  if (f_xorc (tmp, 1) != 0x3d29)
    __builtin_abort ();
  if (f_xorc (tmp, 2) != 0x1101)
    __builtin_abort ();

  if (f_xor (tmp, 0, inv) != 0x1101)
    __builtin_abort ();
  if (f_xor (tmp, -5, inv) != 0x1101)
    __builtin_abort ();
  if (f_xor1 (0) != 0)
    __builtin_abort ();

  return 0;
}
