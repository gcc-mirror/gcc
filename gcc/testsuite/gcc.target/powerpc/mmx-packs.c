/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <mmintrin.h>
#include "mmx-check.h"

#ifndef TEST
#define TEST mmx_test
#endif

static void
__attribute__ ((noinline))
check_packs_pu16 (unsigned long long int src1, unsigned long long int src2,
                  unsigned long long int res_ref)
{
  unsigned long long int res;

  res = (unsigned long long int) _mm_packs_pu16 ((__m64 ) src1, (__m64 ) src2);

  if (res != res_ref)
    abort ();
}

static void
__attribute__ ((noinline))
check_packs_pi16 (unsigned long long int src1, unsigned long long int src2,
                  unsigned long long int res_ref)
{
  unsigned long long int res;

  res = (unsigned long long int) _mm_packs_pi16 ((__m64 ) src1, (__m64 ) src2);


  if (res != res_ref)
    abort ();
}

static void
__attribute__ ((noinline))
check_packs_pi32 (unsigned long long int src1, unsigned long long int src2,
		  unsigned long long int res_ref)
{
  unsigned long long int res;

  res = (unsigned long long int) _mm_packs_pi32 ((__m64 ) src1, (__m64 ) src2);

  if (res != res_ref)
    abort ();
}

static unsigned long long int src1[] =
  { 0xffff0000fffe0000UL, 0x0001000000020000UL, 0xfffffffffffffffeUL,
      0x0000000100000002UL, 0x0001000200030004UL, 0xfffffffefffdfffcUL,
      0x0100020003000400UL, 0xff00fe01fe02fe03UL };

static unsigned long long int src2[] =
  { 0xfffffffdfffffffcUL, 0x0000000200000003UL, 0xfffffffdfffffffcUL,
      0x0000000300000004UL, 0x0005000600070008UL, 0xfffbfffafff9fff8UL,
      0x0005000600070008UL, 0xfffbfffafff9fff8UL };

static unsigned long long int res_pi16[] =
  { 0xfffdfffcff00fe00UL, 0x0002000301000200UL, 0xfffdfffcfffffffeUL,
      0x0003000400010002UL, 0x0506070801020304UL, 0xfbfaf9f8fffefdfcUL,
      0x050607087f7f7f7fUL, 0xfbfaf9f880808080UL };

static unsigned long long int res_pi32[] =
  { 0xfffdfffc80008000UL, 0x000200037fff7fffUL, 0xfffdfffcfffffffeUL,
      0x0003000400010002UL, 0x7fff7fff7fff7fffUL, 0x80008000fffe8000UL,
      0x7fff7fff7fff7fffUL, 0x8000800080008000UL };

static unsigned long long int res_pu16[] =
  { 0x0000000000000000UL, 0x0002000301000200UL, 0x0000000000000000UL,
      0x0003000400010002UL, 0x0506070801020304UL, 0x000000000000000UL,
      0x5060708ffffffffUL, 0x0000000000000000UL };

static void
TEST ()
{
  long i;

  for (i = 0; i < 8; i++)
    {
      check_packs_pu16 (src1[i], src2[i], res_pu16[i]);
      check_packs_pi16 (src1[i], src2[i], res_pi16[i]);
      check_packs_pi32 (src1[i], src2[i], res_pi32[i]);
    }
}

