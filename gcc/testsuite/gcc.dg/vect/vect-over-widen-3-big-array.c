/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

#define N VECTOR_BITS

/* Modified rgb to rgb conversion from FFmpeg.  */
__attribute__ ((noinline)) void
foo (unsigned char *src, unsigned char *dst)
{
  unsigned char *s = src;
  unsigned short *d = (unsigned short *)dst;
  int i;

  for (i = 0; i < N/4; i++)
    {
      const int b = *s++;
      const int g = *s++;
      const int r = *s++;
      const int a = *s++;
      *d = ((b>>3) | ((g&0xFFC)<<3) | ((r+0xF8)>>8) | (a<<9));
      d++;
    }

  s = src;
  d = (unsigned short *)dst;
  for (i = 0; i < N/4; i++)
    {
      const int b = *s++;
      const int g = *s++;
      const int r = *s++;
      const int a = *s++;
      unsigned short expected
	= ((b>>3) | ((g&0xFFC)<<3) | ((r+0xF8)>>8) | (a<<9));
      if (*d != expected)
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

/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* << 3} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 3} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 8} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* << 9} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

