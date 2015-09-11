/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

typedef struct {
   unsigned short a;
   unsigned short b;
} s;

s buffer1[N], buffer2[N];

volatile int y = 0;

__attribute__ ((noinline)) int
main1 (s * __restrict__  pIn, s* __restrict__ pOut)
{
  unsigned short i, x, y, d;
  s *p, *q;

  p = pIn;
  q = pOut;

  for (i = 0; i < N/2; i++)
    {
      x = pIn->a + 5;
      y = pIn->a + 2;
      pOut->a = x;
      pOut->b = pIn->b;
      pOut++;
      pOut->a = y;
      pOut->b = pIn->b;
      pOut++;
      pIn++;
    }

  /* check results:  */
  for (i = 0; i < N/2; i++)
    {
      if (q->a != p->a + 5
          || q->b != p->b)
         abort ();
      q++;
      if (q->a != p->a + 2
          || q->b != p->b)
        abort ();
      q++;
      p++;
    }

  return 0;
}

int main (void)
{
  short i;

  for (i = 0; i < N; i++)
    {
      buffer1[i].a = i;
      buffer1[i].b = i + 8;
      buffer2[i].a = i * 3;
      buffer2[i].b = i * 2;
      if (y) /* Avoid vectorization.  */
        abort ();
    }

  check_vect ();

  main1 (buffer1, buffer2);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_strided2 } } } */
