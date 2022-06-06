/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -mprefer-vector-width=512" } */

typedef int v16si __attribute__((vector_size (64)));
typedef long long v8di __attribute__((vector_size (64)));

void
bar_s32_s64 (v8di * dst, v16si src)
{
  long long tem[8];
  tem[0] = src[0];
  tem[1] = src[1];
  tem[2] = src[2];
  tem[3] = src[3];
  tem[4] = src[4];
  tem[5] = src[5];
  tem[6] = src[6];
  tem[7] = src[7];
  dst[0] = *(v8di *) tem;
}

/* We want to generate
        vpmovsxdq       %ymm0, %zmm0
        vmovdqa64       %zmm0, (%rdi)
        ret
 */
/* { dg-final { scan-assembler "vpmovsxdq" } } */
