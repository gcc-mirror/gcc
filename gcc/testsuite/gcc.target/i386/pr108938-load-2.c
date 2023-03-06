/* { dg-do run } */
/* { dg-options "-O2" } */

#include "pr108938-load-1.c"

int main ()
{
  unsigned char a[8] = {1, 2, 3, 4, 5, 6, 7, 8};
  uint64_t res = swap_rotate_64 (a);
  if (res != 0x0203040506070801ULL)
    __builtin_abort ();

  res = swap_rotate_64_mask_1 (a);
  if (res != 0x0506070801020300ULL)
    __builtin_abort ();

  res = swap_rotate_64_mask_2 (a);
  if (res != 0x0203000506070801ULL)
    __builtin_abort ();

  uint32_t res2 = swap_rotate_32 (a);
  if (res2 != 0x03040102)
    __builtin_abort ();

  res2 = swap_rotate_32_mask_1 (a);
  if (res2 != 0x03040100)
    __builtin_abort ();

  return 0;
}
