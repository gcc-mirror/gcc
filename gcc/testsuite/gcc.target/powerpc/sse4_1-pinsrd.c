/* { dg-do run } */
/* { dg-options "-O2 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>
#include <string.h>

#define msk0 0x00
#define msk1 0x01
#define msk2 0x02
#define msk3 0x03

static void
TEST (void)
{
  union
    {
      __m128i x;
      unsigned int i[4];
    } res [4], val, tmp;
  static unsigned int ins[4] = { 3, 4, 5, 6 };
  int masks[4];
  int i;

  val.i[0] = 55;
  val.i[1] = 55;
  val.i[2] = 55;
  val.i[3] = 55;

  /* Check pinsrd imm8, r32, xmm.  */
  res[0].x = _mm_insert_epi32 (val.x, ins[0], msk0);
  res[1].x = _mm_insert_epi32 (val.x, ins[0], msk1);
  res[2].x = _mm_insert_epi32 (val.x, ins[0], msk2);
  res[3].x = _mm_insert_epi32 (val.x, ins[0], msk3);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;

  for (i = 0; i < 4; i++)
    {
      tmp.x = val.x;
      tmp.i[masks[i]] = ins[0];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
    
  /* Check pinsrd imm8, m32, xmm.  */
  for (i = 0; i < 4; i++)
    {
      res[i].x = _mm_insert_epi32 (val.x, ins[i], msk0);
      masks[i] = msk0;
    }

  for (i = 0; i < 4; i++)
    {
      tmp.x = val.x;
      tmp.i[masks[i]] = ins[i];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
}
