/* { dg-do run } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
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
#define msk4 0x04
#define msk5 0x05
#define msk6 0x06
#define msk7 0x07
#define msk8 0x08
#define msk9 0x09
#define mskA 0x0A
#define mskB 0x0B
#define mskC 0x0C
#define mskD 0x0D
#define mskE 0x0E
#define mskF 0x0F

static void
TEST (void)
{
  union
    {
      __m128i x;
      unsigned int i[4];
      unsigned char c[16];
    } res [16], val, tmp;
  int masks[16];
  unsigned char ins[4] = { 3, 4, 5, 6 };
  int i;

  val.i[0] = 0x35251505;
  val.i[1] = 0x75655545;
  val.i[2] = 0xB5A59585;
  val.i[3] = 0xF5E5D5C5;

  /* Check pinsrb imm8, r32, xmm.  */
  res[0].x = _mm_insert_epi8 (val.x, ins[0], msk0);
  res[1].x = _mm_insert_epi8 (val.x, ins[0], msk1);
  res[2].x = _mm_insert_epi8 (val.x, ins[0], msk2);
  res[3].x = _mm_insert_epi8 (val.x, ins[0], msk3);
  res[4].x = _mm_insert_epi8 (val.x, ins[0], msk4);
  res[5].x = _mm_insert_epi8 (val.x, ins[0], msk5);
  res[6].x = _mm_insert_epi8 (val.x, ins[0], msk6);
  res[7].x = _mm_insert_epi8 (val.x, ins[0], msk7);
  res[8].x = _mm_insert_epi8 (val.x, ins[0], msk8);
  res[9].x = _mm_insert_epi8 (val.x, ins[0], msk9);
  res[10].x = _mm_insert_epi8 (val.x, ins[0], mskA);
  res[11].x = _mm_insert_epi8 (val.x, ins[0], mskB);
  res[12].x = _mm_insert_epi8 (val.x, ins[0], mskC);
  res[13].x = _mm_insert_epi8 (val.x, ins[0], mskD);
  res[14].x = _mm_insert_epi8 (val.x, ins[0], mskE);
  res[15].x = _mm_insert_epi8 (val.x, ins[0], mskF);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;
  masks[4] = msk4;
  masks[5] = msk5;
  masks[6] = msk6;
  masks[7] = msk7;
  masks[8] = msk8;
  masks[9] = msk9;
  masks[10] = mskA;
  masks[11] = mskB;
  masks[12] = mskC;
  masks[13] = mskD;
  masks[14] = mskE;
  masks[15] = mskF;

  for (i = 0; i < 16; i++)
    {
      tmp.x = val.x;
      tmp.c[masks[i]] = ins[0];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
    
  /* Check pinsrb imm8, m8, xmm.  */
  for (i = 0; i < 16; i++)
    {
      res[i].x = _mm_insert_epi8 (val.x, ins[i % 4], msk0);
      masks[i] = msk0;
    }

  for (i = 0; i < 16; i++)
    {
      tmp.x = val.x;
      tmp.c[masks[i]] = ins[i % 4];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
}
