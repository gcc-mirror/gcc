/* { dg-do run } */
/* { dg-options "-O2" } */

#include "pr108938-1.c"

int main ()
{
  uint64_t a = 0x0807060504030201ULL;
  uint64_t res = swap_rotate_64 (a);
  if (res != 0x0203040506070801ULL)
    __builtin_abort ();

  res = swap_rotate_64_mask_1 (a);
  if (res != 0x0203040506070800ULL)
    __builtin_abort ();

  res = swap_rotate_64_mask_2 (a);
  if (res != 0x0203040506070001ULL)
    __builtin_abort ();

  uint32_t b = 0x04030201;
  uint32_t res2 = swap_rotate_32 (b);
  if (res2 != 0x03040102)
    __builtin_abort ();

  res2 = swap_rotate_32_mask_1 (b);
  if (res2 != 0x03040002)
    __builtin_abort ();

  res2 = swap_rotate_32_mask_2 (b);
  if (res2 != 0x03040100)
    __builtin_abort ();

  return 0;
}
