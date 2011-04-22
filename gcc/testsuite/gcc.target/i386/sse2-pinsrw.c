/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -msse2" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>
#include <string.h>

#define msk0 0x00
#define msk1 0x01
#define msk2 0x02
#define msk3 0x03
#define msk4 0x04
#define msk5 0x05
#define msk6 0x06
#define msk7 0x07

static void
TEST (void)
{
  union
    {
      __m128i x;
      unsigned int i[4];
      unsigned short s[8];
    } res [8], val, tmp;
  int masks[8];
  unsigned short ins[4] = { 3, 4, 5, 6 };
  int i;

  val.i[0] = 0x35251505;
  val.i[1] = 0x75655545;
  val.i[2] = 0xB5A59585;
  val.i[3] = 0xF5E5D5C5;

  /* Check pinsrw imm8, r32, xmm.  */
  res[0].x = _mm_insert_epi16 (val.x, ins[0], msk0);
  res[1].x = _mm_insert_epi16 (val.x, ins[0], msk1);
  res[2].x = _mm_insert_epi16 (val.x, ins[0], msk2);
  res[3].x = _mm_insert_epi16 (val.x, ins[0], msk3);
  res[4].x = _mm_insert_epi16 (val.x, ins[0], msk4);
  res[5].x = _mm_insert_epi16 (val.x, ins[0], msk5);
  res[6].x = _mm_insert_epi16 (val.x, ins[0], msk6);
  res[7].x = _mm_insert_epi16 (val.x, ins[0], msk7);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;
  masks[4] = msk4;
  masks[5] = msk5;
  masks[6] = msk6;
  masks[7] = msk7;

  for (i = 0; i < 8; i++)
    {
      tmp.x = val.x;
      tmp.s[masks[i]] = ins[0];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
    
  /* Check pinsrw imm8, m16, xmm.  */
  for (i = 0; i < 8; i++)
    {
      res[i].x = _mm_insert_epi16 (val.x, ins[i % 2], msk0);
      masks[i] = msk0;
    }

  for (i = 0; i < 8; i++)
    {
      tmp.x = val.x;
      tmp.s[masks[i]] = ins[i % 2];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
}
