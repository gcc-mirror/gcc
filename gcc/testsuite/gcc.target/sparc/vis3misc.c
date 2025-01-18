/* { dg-do compile } */
/* { dg-options "-mvis3b" } */

typedef int __v2si __attribute__((vector_size(8)));
typedef short __v4hi __attribute__((vector_size(8)));
typedef unsigned char __v8qi __attribute__((vector_size(8)));
typedef long long int64_t;

__v4hi test_fchksm16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fchksm16 (x, y);
}

long test_pdistn (__v8qi x, __v8qi y)
{
  return __builtin_vis_pdistn (x, y);
}

__v4hi test_fmean16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fmean16 (x, y);
}

int64_t test_fpadd64 (int64_t x, int64_t y)
{
  return __builtin_vis_fpadd64 (x, y);
}

int64_t test_fpsub64 (int64_t x, int64_t y)
{
  return __builtin_vis_fpsub64 (x, y);
}

/* { dg-final { scan-assembler "fchksm16\t%" } } */
/* { dg-final { scan-assembler "pdistn\t%" } } */
/* { dg-final { scan-assembler "fmean16\t%" } } */
/* { dg-final { scan-assembler "fpadd64\t%" } } */
/* { dg-final { scan-assembler "fpsub64\t%" } } */
