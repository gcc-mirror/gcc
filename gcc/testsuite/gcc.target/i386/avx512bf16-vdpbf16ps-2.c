/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O2" } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

typedef union
{
  __m512 x;
  float a[16];
} union512s;

float res_ref[16];
union512s res;
__m512bh x1, x2;
__mmask16 m16;

static void __attribute__((noinline, unused))
merge_masking_s (float *arr, unsigned long long mask, int size)
{
  int i;
  for (i = 0; i < size; i++)
  {
    arr[i] = (mask & (1LL << i)) ? arr[i] : 117;
  }
}

static int __attribute__((noinline, unused))
check_union512s (union512s u, const float *v)
{
  int i;
  int err = 0;
  for (i = 0; i < (sizeof (u.a) / sizeof ((u.a)[0])); i++)
    if (u.a[i] != v[i])
    {
      err++;
      ;
    }
  return err;
}

void extern
avx512bf16_test (void)
{
  res.x = _mm512_mask_dpbf16_ps (res.x, m16, x1, x2);
  merge_masking_s (res_ref, m16, 16);
  if (check_union512s (res, res_ref))
    abort ();
}
