/* PR rtl-optimization/89445 */
/* { dg-do run { target { avx512f && mmap } } } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

#include <sys/mman.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
#ifndef MAP_ANON
#define MAP_ANON 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

__attribute__ ((noipa))
void daxpy (unsigned long n, double a, double const *__restrict x,
	    double *__restrict y)
{
  const __m512d v_a = _mm512_broadcastsd_pd (_mm_set_sd (a));
  const __mmask16 final = (1U << (n % 8u)) - 1;
  __mmask16 mask = 65535u;
  unsigned long i;
  for (i = 0; i < n * sizeof (double); i += 8 * sizeof (double))
    {
      if (i + 8 * sizeof (double) > n * sizeof (double))
	mask = final;
      __m512d v_x = _mm512_maskz_loadu_pd (mask, (char const *) x + i);
      __m512d v_y = _mm512_maskz_loadu_pd (mask, (char const *) y + i);
      __m512d tmp = _mm512_fmadd_pd (v_x, v_a, v_y);
      _mm512_mask_storeu_pd ((char *) y + i, mask, tmp);
    }
}

static const double x[] = { 1, 2, 3, 4 };

static void
avx512f_test (void)
{
  char *ptr
    = (char *) mmap (NULL, 2 * 4096, PROT_READ | PROT_WRITE,
		     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (ptr == MAP_FAILED)
    return;

  munmap (ptr + 4096, 4096);
  double *y = (double *) (ptr + 4096 - sizeof (x));
  __builtin_memcpy (y, x, sizeof (x));
  daxpy (sizeof (x) / sizeof (x[0]), 1.0, x, y);
  if (y[0] != 2.0 || y[1] != 4.0 || y[2] != 6.0 || y[3] != 8.0)
    abort ();
}
