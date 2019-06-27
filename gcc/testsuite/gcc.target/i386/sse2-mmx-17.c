/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_set (char i0, char i1, char i2, char i3,
	  char i4, char i5, char i6, char i7, long long *r)
{
  *(__m64 *) r = _mm_set_pi8 (i0, i1, i2, i3, i4, i5, i6, i7);
}

/* Routine to manually compute the results */
static void
compute_correct_result (char i0, char i1, char i2, char i3,
			char i4, char i5, char i6, char i7,
			long long *res_p)
{
  char *res = (char *) res_p;
  res[0] = i7;
  res[1] = i6;
  res[2] = i5;
  res[3] = i4;
  res[4] = i3;
  res[5] = i2;
  res[6] = i1;
  res[7] = i0;
}

static void
sse2_test (void)
{
  char i0, i1, i2, i3, i4, i5, i6, i7;
  long long r, ck;

  /* Run the MMX tests */
  i0 = 0x12;
  i1 = 0x34;
  i2 = 0x56;
  i3 = 0x78;
  i4 = 0x90;
  i5 = 0xab;
  i6 = 0xcd;
  i7 = 0xef;
  test_set (i0, i1, i2, i3, i4, i5, i6, i7, &r);
  compute_correct_result (i0, i1, i2, i3, i4, i5, i6, i7, &ck);
  if (ck != r)
    abort ();
}
