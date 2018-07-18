/* { dg-require-effective-target vect_float } */

#include "tree-vect.h"

void __attribute__((noinline,noclone))
downscale_2 (const float* src, int src_n, float* dst)
{
  int i;

  for (i = 0; i < src_n; i += 2) {
      const float* a = src;
      const float* b = src + 4;

      dst[0] = (a[0] + b[0]) / 2;
      dst[1] = (a[1] + b[1]) / 2;
      dst[2] = (a[2] + b[2]) / 2;
      dst[3] = (a[3] + b[3]) / 2;

      src += 2 * 4;
      dst +=     4;
  }
}

int main ()
{
  const float in[4 * 4] = {
      1, 2, 3, 4,
      5, 6, 7, 8,

      1, 2, 3, 4,
      5, 6, 7, 8
  };
  float out[2 * 4];

  check_vect ();

  downscale_2 (in, 4, out);

  if (out[0] != 3 || out[1] != 4 || out[2] != 5 || out[3] != 6
      || out[4] != 3 || out[5] != 4 || out[6] != 5 || out[7] != 6)
    __builtin_abort ();
  
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
