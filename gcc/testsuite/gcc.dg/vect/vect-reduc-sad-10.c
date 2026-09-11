/* { dg-require-effective-target vect_usad_char } */

#include "tree-vect.h"

#define H 16
#define STRIDE 11

unsigned char a[H * STRIDE + 1];
unsigned char b[H * STRIDE + 1];

/* Half-pel motion estimation as found in media codecs: the second SAD
   operand averages two overlapping runs of one nine-element group.  */

#define TERM(K) \
  s += __builtin_abs (p1[K] - ((p2[K] + p2[K + 1] + 1) >> 1))

int __attribute__ ((noipa))
f (unsigned char *p1, unsigned char *p2, int stride, int h)
{
  int s = 0;
  for (int i = 0; i < h; i++)
    {
      TERM (0); TERM (1); TERM (2); TERM (3);
      TERM (4); TERM (5); TERM (6); TERM (7);
      p1 += stride;
      p2 += stride;
    }
  return s;
}

int
main (void)
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < H * STRIDE + 1; i++)
    {
      a[i] = (i * 17 + 3) & 0xff;
      b[i] = (i * 251 + 91) & 0xff;
    }

  int expected = 0;
#pragma GCC novector
  for (int i = 0; i < H; i++)
    {
      unsigned char *p1 = a + i * STRIDE;
      unsigned char *p2 = b + i * STRIDE;
#pragma GCC novector
      for (int k = 0; k < 8; k++)
	expected += __builtin_abs (p1[k] - ((p2[k] + p2[k + 1] + 1) >> 1));
    }

  if (f (a, b, STRIDE, H) != expected)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_sad_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
