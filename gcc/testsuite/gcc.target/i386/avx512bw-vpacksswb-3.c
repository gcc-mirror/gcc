/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

static char
short_to_byte (short iVal)
{
  short sVal;

  if (iVal < -128)
    sVal = -128;
  else if (iVal > 127)
    sVal = 127;
  else
    sVal = iVal;

  return sVal;
}

void
TEST (void)
{
  union512i_w s1, s2;
  union512i_b res1;
  char dst_ref[64];
  int i;

  s1.x = _mm512_set_epi16 (1, 2, 3, 4, 650, 20, 30, 90, 88, 44, 33, 22, 11, 98, 76, -650,
			   128, 230, -112, -128, -3, -4, -7, 9, 10, 11, 12, 13, -223, 10, 8, 11);
  s2.x = _mm512_set_epi16 (80, 40, 31, 21, 10, 99, 74, -650, 2, 3, 4, 5, 650, 21, 31, 91,
			   280, -140, 310, 20, 9, 98, 73, -651, 3, 4, 5, 6, 651, 22, 32, 92);
  res1.x = _mm512_packs_epi16 (s1.x, s2.x);
  for (int i = 0; i != 8; i++)
    {
      dst_ref[i] = short_to_byte (s1.a[i]);
      dst_ref[i + 8] = short_to_byte (s2.a[i]);
      dst_ref[i + 16] = short_to_byte (s1.a[i + 8]);
      dst_ref[i + 24] = short_to_byte (s2.a[i + 8]);
      dst_ref[i + 32] = short_to_byte (s1.a[i + 16]);
      dst_ref[i + 40] = short_to_byte (s2.a[i + 16]);
      dst_ref[i + 48] = short_to_byte (s1.a[i + 24]);
      dst_ref[i + 56] = short_to_byte (s2.a[i + 24]);      
    }

  if (check_union512i_b (res1, dst_ref))
    abort ();
}
