/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 64

unsigned char dst[N], src[N + 1];
short sub[N];

__attribute__ ((noipa)) void
ang_row (unsigned char *__restrict d, const unsigned char *__restrict s,
	 int angle, int n)
{
  int f = angle & 31;
  int a = 32 - f;
  for (int x = 0; x < n; x++)
    d[x] = (unsigned char) ((a * s[x] + f * s[x + 1] + 16) >> 5);
}

__attribute__ ((noipa)) void
sub_row (short *__restrict d, const unsigned char *__restrict s,
	 int angle, int n)
{
  int f = angle & 31;
  /* Walk D with a pointer: indexing it would scale the index by 2 and give
     the test a widening multiply that has nothing to do with the patch.  */
  for (const unsigned char *e = s + n; s < e; s++)
    *d++ = (short) (f - *s);
}

int
main (void)
{
  int i;

  check_vect ();

#pragma GCC novector
  for (i = 0; i < N + 1; i++)
    src[i] = (i * 37 + 11) & 0xff;

  ang_row (dst, src, 0x123, N);
  sub_row (sub, src, 0x123, N);

#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      int f = 0x123 & 31, a = 32 - f;
      if (dst[i] != (unsigned char) ((a * src[i] + f * src[i + 1] + 16) >> 5))
	abort ();
      if (sub[i] != (short) (f - src[i]))
	abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_widen_mult_pattern: detected" "vect" { target vect_widen_mult_qi_to_hi } } } */
/* { dg-final { scan-tree-dump "vect_recog_widen_minus_pattern: detected" "vect" { target vect_widen_mult_qi_to_hi } } } */
