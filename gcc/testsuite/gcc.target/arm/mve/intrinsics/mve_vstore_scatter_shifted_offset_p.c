/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t __p;
int
foowu32( uint32_t * pDataSrc, uint32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrwq_z_u32 ((uint32_t const *) pDataSrc, __p);
    uint32x4_t vecIn2 = vldrwq_z_u32 ((uint32_t const *) &pDataSrc[4], __p);
    vstrwq_scatter_shifted_offset_p_u32 (pDataDest, vecOffs1, vecIn1, __p);
    vstrwq_scatter_shifted_offset_p_u32 (pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foowf32( float32_t * pDataSrc, float32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    float32x4_t vecIn1 = vldrwq_z_f32 ((float32_t const *) pDataSrc, __p);
    float32x4_t vecIn2 = vldrwq_z_f32 ((float32_t const *) &pDataSrc[4], __p);
    vstrwq_scatter_shifted_offset_p_f32 (pDataDest, vecOffs1, vecIn1, __p);
    vstrwq_scatter_shifted_offset_p_f32 (pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foohu16( uint16_t * pDataSrc, uint16_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 9, 11, 13, 10, 12, 15, 8, 14};
    uint16x8_t vecIn1 = vldrhq_z_u16 ((uint16_t const *) pDataSrc, __p);
    uint16x8_t vecIn2 = vldrhq_z_u16 ((uint16_t const *) &pDataSrc[8], __p);
    vstrhq_scatter_shifted_offset_p_u16 (pDataDest, vecOffs1, vecIn1, __p);
    vstrhq_scatter_shifted_offset_p_u16 (pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foohu32( uint32_t * pDataSrc, uint32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrhq_z_u32 ((uint16_t const *) pDataSrc, __p);
    uint32x4_t vecIn2 = vldrhq_z_u32 ((uint16_t const *) &pDataSrc[4], __p);
    vstrhq_scatter_shifted_offset_p_u32 ((uint16_t *)pDataDest, vecOffs1, vecIn1, __p);
    vstrhq_scatter_shifted_offset_p_u32 ((uint16_t *)pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foohf16( float16_t * pDataSrc, float16_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 9, 11, 13, 10, 12, 15, 8, 14};
    float16x8_t vecIn1 = vldrhq_z_f16 ((float16_t const *) pDataSrc, __p);
    float16x8_t vecIn2 = vldrhq_z_f16 ((float16_t const *) &pDataSrc[8], __p);
    vstrhq_scatter_shifted_offset_p_f16 (pDataDest, vecOffs1, vecIn1, __p);
    vstrhq_scatter_shifted_offset_p_f16 (pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foodu64( uint64_t * pDataSrc, uint64_t * pDataDest)
{
    const uint64x2_t vecOffs1 = { 0, 1};
    const uint64x2_t vecOffs2 = { 2, 3};
    uint32x4_t vecIn1 = vldrwq_z_u32 ((uint32_t const *) pDataSrc, __p);
    uint32x4_t vecIn2 = vldrwq_z_u32 ((uint32_t const *) &pDataSrc[2], __p);

    vstrdq_scatter_shifted_offset_p_u64 (pDataDest, vecOffs1, (uint64x2_t) vecIn1, __p);
    vstrdq_scatter_shifted_offset_p_u64 (pDataDest, vecOffs2, (uint64x2_t) vecIn2, __p);

    pDataDest[2] = pDataSrc[2];
    return 0;
}

int
foows32( int32_t * pDataSrc, int32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    int32x4_t vecIn1 = vldrwq_z_s32 ((int32_t const *) pDataSrc, __p);
    int32x4_t vecIn2 = vldrwq_z_s32 ((int32_t const *) &pDataSrc[4], __p);
    vstrwq_scatter_shifted_offset_p_s32 (pDataDest, vecOffs1, vecIn1, __p);
    vstrwq_scatter_shifted_offset_p_s32 (pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foohs16( int16_t * pDataSrc, int16_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 9, 11, 13, 10, 12, 15, 8, 14};
    int16x8_t vecIn1 = vldrhq_z_s16 ((int16_t const *) pDataSrc, __p);
    int16x8_t vecIn2 = vldrhq_z_s16 ((int16_t const *) &pDataSrc[8], __p);
    vstrhq_scatter_shifted_offset_p_s16 (pDataDest, vecOffs1, vecIn1, __p);
    vstrhq_scatter_shifted_offset_p_s16 (pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foohs32( int32_t * pDataSrc, int32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    int32x4_t vecIn1 = vldrhq_z_s32 ((int16_t const *) pDataSrc, __p);
    int32x4_t vecIn2 = vldrhq_z_s32 ((int16_t const *) &pDataSrc[4], __p);
    vstrhq_scatter_shifted_offset_p_s32 ((int16_t *)pDataDest, vecOffs1, vecIn1, __p);
    vstrhq_scatter_shifted_offset_p_s32 ((int16_t *)pDataDest, vecOffs2, vecIn2, __p);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foods64( int64_t * pDataSrc, int64_t * pDataDest)
{
    const uint64x2_t vecOffs1 = { 0, 1};
    const uint64x2_t vecOffs2 = { 2, 3};
    int32x4_t vecIn1 = vldrwq_z_s32 ((int32_t const *) pDataSrc, __p);
    int32x4_t vecIn2 = vldrwq_z_s32 ((int32_t const *) &pDataSrc[2], __p);

    vstrdq_scatter_shifted_offset_p_s64 (pDataDest, vecOffs1, (int64x2_t) vecIn1, __p);
    vstrdq_scatter_shifted_offset_p_s64 (pDataDest, vecOffs2, (int64x2_t) vecIn2, __p);

    pDataDest[2] = pDataSrc[2];
    return 0;
}

/* { dg-final { scan-assembler-times "vstr\[a-z\]t" 20 } } */
