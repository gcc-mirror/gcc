/* { dg-do compile } */
/* { dg-options "-O3 -mavx512fp16 -mavx512vl -ffast-math" } */
/* { dg-final { scan-assembler "vrcpsh.*\n.*vmulsh" } } */
/* { dg-final { scan-assembler "vrcpph.*\n.*vmulph" } } */
/* { dg-final { scan-assembler "vrsqrtsh.*\n.*vmulsh" } } */
/* { dg-final { scan-assembler "vrsqrtph.*\n.*vmulph" } } */
/* { dg-final { scan-assembler-not "vsqrtsh" } } */
/* { dg-final { scan-assembler-not "vsqrtph" } } */
/* { dg-final { scan-assembler-not "vdivsh" } } */
/* { dg-final { scan-assembler-not "vdivph" } } */

#define FAST_ATTR \
  __attribute__((noinline, noclone, optimize("fast-math"), target("recip")))

_Float16 FAST_ATTR
scalar_hf_rcp_fast (_Float16 a, _Float16 b)
{
  return a / b;
}

_Float16 FAST_ATTR
scalar_hf_rsqrt_fast (_Float16 a, _Float16 b)
{
  return a / __builtin_sqrtf16 (b);
}

void FAST_ATTR
vector_hf_rcp_fast (_Float16 * restrict a, _Float16 * restrict b,
		    _Float16 * restrict c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    c[i] = a[i] / b[i];
}

void FAST_ATTR
vector_hf_rsqrt_fast (_Float16 * restrict a, _Float16 * restrict b,
		    _Float16 * restrict c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    c[i] = a[i] / __builtin_sqrtf16(b[i]);
}
