/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 8 / 16)
#else
#define N 64
#endif

/* Modified rgb to rgb conversion from FFmpeg.  */
__attribute__ ((noinline)) int
foo (unsigned char *src, unsigned char *dst)
{
  unsigned char *s = src;
  unsigned short *d = (unsigned short *)dst, res;
  int i, result = 0;

  for (i = 0; i < N/4; i++)
    {
      const int b = *s++;
      const int g = *s++;
      const int r = *s++;
      const int a = *s++;
      res = ((b>>3) | ((g&0xFC)<<3) | ((r&0xF8)<<8) | (a>>5));
      *d = res;
      result += res;
      d++;
    }

  s = src;
  d = (unsigned short *)dst;
#pragma GCC novector
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

  return result;
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

/* { dg-final { scan-tree-dump-times "vect_recog_widen_shift_pattern: detected" 2 "vect" { target vect_widen_shift } } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 3} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 5} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

