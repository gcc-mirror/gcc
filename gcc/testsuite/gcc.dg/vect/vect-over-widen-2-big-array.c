/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 512

/* Modified rgb to rgb conversion from FFmpeg.  */
__attribute__ ((noinline)) void
foo (unsigned char *src, unsigned char *dst)
{
  unsigned char *s = src;
  int *d = (int *)dst;
  int i;

  for (i = 0; i < N/4; i++)
    {
      const int b = *s++;
      const int g = *s++;
      const int r = *s++;
      const int a = *s++;
      *d = ((b>>3) | ((g&0xFC)<<3) | ((r&0xF8)<<8) | (a>>5));
      d++;
    }

  s = src;
  d = (int *)dst;
  for (i = 0; i < N/4; i++)
    {
      const int b = *s++;
      const int g = *s++;
      const int r = *s++;
      const int a = *s++;
      if (*d != ((b>>3) | ((g&0xFC)<<3) | ((r&0xF8)<<8) | (a>>5)))
        abort ();
      d++;
    }
}

int main (void)
{
  int i;
  unsigned char in[N], out[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in[i] = i;
      out[i] = 255;
      __asm__ volatile ("");
    }

  foo (in, out);

  return 0;
}

/* Final value stays in int, so no over-widening is detected at the moment.  */
/* { dg-final { scan-tree-dump-times "vect_recog_over_widening_pattern: detected" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

