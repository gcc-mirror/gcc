/* { dg-do compile } */
/* { dg-options "-msse2 -mno-sse4.1 -O3 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 5 "vect" } } */
#include<stdint.h>
void
vec_widen_smul8 (int16_t* __restrict v3, int8_t *v1, int8_t *v2, int order)
{
  while (order--)
    *v3++ = (int16_t) *v1++ * *v2++;
}

void
vec_widen_umul8(uint16_t* __restrict v3, uint8_t *v1, uint8_t *v2, int order)
{
  while (order--)
    *v3++ = (uint16_t) *v1++ * *v2++;
}

void
vec_widen_smul16(int32_t* __restrict v3, int16_t *v1, int16_t *v2, int order)
{
  while (order--)
    *v3++ = (int32_t) *v1++ * *v2++;
}

void
vec_widen_umul16(uint32_t* __restrict v3, uint16_t *v1, uint16_t *v2, int order)
{
  while (order--)
    *v3++ = (uint32_t) *v1++ * *v2++;
}

void
vec_widen_smul32(int64_t* __restrict v3, int32_t *v1, int32_t *v2, int order)
{
  while (order--)
    *v3++ = (int64_t) *v1++ * *v2++;
}

void
vec_widen_umul32(uint64_t* __restrict v3, uint32_t *v1, uint32_t *v2, int order)
{
  while (order--)
    *v3++ = (uint64_t) *v1++ * *v2++;
}
