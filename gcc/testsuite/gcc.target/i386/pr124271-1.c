/* PR target/124271 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mno-avx512f -fdump-tree-optimized" } */

typedef __UINT64_TYPE__ uint64_t;
typedef __UINT32_TYPE__ uint32_t;

static inline uint64_t
mul32 (uint64_t a, uint64_t b)
{
  return (uint64_t) (uint32_t) a * (uint64_t) (uint32_t) b;
}

void
foo (uint64_t *__restrict a, const uint64_t *__restrict b)
{
  for (int i = 0; i < 1024; i++)
    a[i] = mul32 (a[i], b[i]);
}

void
bar (long long *__restrict a, const long long *__restrict b)
{
  for (int i = 0; i < 1024; i++)
    a[i] = (a[i] & 0xffffffff) * (b[i] & 0xffffffff);
}

/* { dg-final { scan-tree-dump-times "WIDEN_MULT_EVEN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-assembler-times "vpmuludq\[ \\t\]+\[^\n\]*%ymm" 2 } } */
/* { dg-final { scan-assembler-not "vpsrlq" } } */
/* { dg-final { scan-assembler-not "vpsllq" } } */
/* { dg-final { scan-assembler-not "vpand" } } */
