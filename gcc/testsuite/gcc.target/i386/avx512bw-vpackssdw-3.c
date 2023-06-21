/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define DST_SIZE (AVX512F_LEN / 16)
#define SRC_SIZE (AVX512F_LEN / 32)

#include "limits.h"

#include "avx512f-mask-type.h"

static short
int_to_short (int iVal)
{
  short sVal;

  if (iVal < -32768)
    sVal = -32768;
  else if (iVal > 32767)
    sVal = 32767;
  else
    sVal = iVal;

  return sVal;
}

void
TEST (void)
{
  union512i_d s1, s2;
  union512i_w res1;
  short dst_ref[32];
  int i;

  s1.x = _mm512_set_epi32 (1, 2, 3, 4, 65000, 20, 30, 90, 88, 44, 33, 22, 11, 98, 76, -65000);
  s2.x = _mm512_set_epi32 (80, 40, 31, 21, 10, 99, 74, -65000, 2, 3, 4, 5, 65010, 21, 31, 91);
  res1.x = _mm512_packs_epi32 (s1.x, s2.x);
  for (int i = 0; i != 4; i++)
    {
      dst_ref[i] = int_to_short (s1.a[i]);
      dst_ref[i + 4] = int_to_short (s2.a[i]);
      dst_ref[i + 8] = int_to_short (s1.a[i + 4]);
      dst_ref[i + 12] = int_to_short (s2.a[i + 4]);
      dst_ref[i + 16] = int_to_short (s1.a[i + 8]);
      dst_ref[i + 20] = int_to_short (s2.a[i + 8]);
      dst_ref[i + 24] = int_to_short (s1.a[i + 12]);
      dst_ref[i + 28] = int_to_short (s2.a[i + 12]);      
    }

  if (check_union512i_w (res1, dst_ref))
    abort ();
}
