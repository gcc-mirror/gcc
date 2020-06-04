/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int
foobu8( uint8_t * pDataSrc, uint8_t * pDataDest)
{
    const uint8x16_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5, 9, 11, 13, 10, 12, 15, 8, 14};
    const uint8x16_t vecOffs2 = { 31, 29, 27, 25, 23, 28, 21, 26, 19, 24, 17, 22, 16, 20, 18, 30};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[16]);
    vstrbq_scatter_offset_u8 (pDataDest, vecOffs1, (uint8x16_t) vecIn1);
    vstrbq_scatter_offset_u8 (pDataDest, vecOffs2, (uint8x16_t) vecIn2);
    pDataDest[32] = pDataSrc[32];
    return 0;
}

int
foobu16( uint8_t * pDataSrc, uint8_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 11, 13, 10, 12, 15, 8, 14, 9};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[8]);
    vstrbq_scatter_offset_u16 (pDataDest, vecOffs1, (uint16x8_t) vecIn1);
    vstrbq_scatter_offset_u16 (pDataDest, vecOffs2, (uint16x8_t) vecIn2);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foobu32( uint8_t * pDataSrc, uint8_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[4]);
    vstrbq_scatter_offset_u32 (pDataDest, vecOffs1, (uint32x4_t) vecIn1);
    vstrbq_scatter_offset_u32 (pDataDest, vecOffs2, (uint32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foobs8( int8_t * pDataSrc, int8_t * pDataDest)
{
    const uint8x16_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5, 9, 11, 13, 10, 12, 15, 8, 14};
    const uint8x16_t vecOffs2 = { 31, 29, 27, 25, 23, 28, 21, 26, 19, 24, 17, 22, 16, 20, 18, 30};
    int32x4_t vecIn1 = vldrwq_s32 ((int32_t const *) pDataSrc);
    int32x4_t vecIn2 = vldrwq_s32 ((int32_t const *) &pDataSrc[16]);
    vstrbq_scatter_offset_s8 (pDataDest, vecOffs1, (int8x16_t) vecIn1);
    vstrbq_scatter_offset_s8 (pDataDest, vecOffs2, (int8x16_t) vecIn2);
    pDataDest[32] = pDataSrc[32];
    return 0;
}

int
foobs16( int8_t * pDataSrc, int8_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 11, 13, 10, 12, 15, 8, 14, 9};
    int32x4_t vecIn1 = vldrwq_s32 ((int32_t const *) pDataSrc);
    int32x4_t vecIn2 = vldrwq_s32 ((int32_t const *) &pDataSrc[8]);
    vstrbq_scatter_offset_s16 (pDataDest, vecOffs1, (int16x8_t) vecIn1);
    vstrbq_scatter_offset_s16 (pDataDest, vecOffs2, (int16x8_t) vecIn2);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foobs32( uint8_t * pDataSrc, int8_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    int32x4_t vecIn1 = vldrwq_s32 ((int32_t const *) pDataSrc);
    int32x4_t vecIn2 = vldrwq_s32 ((int32_t const *) &pDataSrc[4]);
    vstrbq_scatter_offset_s32 (pDataDest, vecOffs1, (int32x4_t) vecIn1);
    vstrbq_scatter_offset_s32 (pDataDest, vecOffs2, (int32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foohu16( uint16_t * pDataSrc, uint16_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 11, 13, 10, 12, 15, 8, 14, 9};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[8]);
    vstrhq_scatter_offset_u16 (pDataDest, vecOffs1, (uint16x8_t) vecIn1);
    vstrhq_scatter_offset_u16 (pDataDest, vecOffs2, (uint16x8_t) vecIn2);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foohu32( uint16_t * pDataSrc, uint16_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[4]);
    vstrhq_scatter_offset_u32 (pDataDest, vecOffs1, (uint32x4_t) vecIn1);
    vstrhq_scatter_offset_u32 (pDataDest, vecOffs2, (uint32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foohs16( int16_t * pDataSrc, int16_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 11, 13, 10, 12, 15, 8, 14, 9};
    int32x4_t vecIn1 = vldrwq_s32 ((int32_t const *) pDataSrc);
    int32x4_t vecIn2 = vldrwq_s32 ((int32_t const *) &pDataSrc[8]);
    vstrhq_scatter_offset_s16 (pDataDest, vecOffs1, (int16x8_t) vecIn1);
    vstrhq_scatter_offset_s16 (pDataDest, vecOffs2, (int16x8_t) vecIn2);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foohs32( uint16_t * pDataSrc, int16_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    int32x4_t vecIn1 = vldrwq_s32 ((int32_t const *) pDataSrc);
    int32x4_t vecIn2 = vldrwq_s32 ((int32_t const *) &pDataSrc[4]);
    vstrhq_scatter_offset_s32 (pDataDest, vecOffs1, (int32x4_t) vecIn1);
    vstrhq_scatter_offset_s32 (pDataDest, vecOffs2, (int32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foohf16( float16_t * pDataSrc, float16_t * pDataDest)
{
    const uint16x8_t vecOffs1 = { 0, 3, 6, 1, 4, 7, 2, 5};
    const uint16x8_t vecOffs2 = { 11, 13, 10, 12, 15, 8, 14, 9};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[8]);
    vstrhq_scatter_offset_f16 (pDataDest, vecOffs1, (float16x8_t) vecIn1);
    vstrhq_scatter_offset_f16 (pDataDest, vecOffs2, (float16x8_t) vecIn2);
    pDataDest[16] = pDataSrc[16];
    return 0;
}

int
foowu32( uint32_t * pDataSrc, uint32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[4]);
    vstrwq_scatter_offset_u32 (pDataDest, vecOffs1, (uint32x4_t) vecIn1);
    vstrwq_scatter_offset_u32 (pDataDest, vecOffs2, (uint32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foows32( int32_t * pDataSrc, int32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[4]);
    vstrwq_scatter_offset_s32 (pDataDest, vecOffs1, (int32x4_t) vecIn1);
    vstrwq_scatter_offset_s32 (pDataDest, vecOffs2, (int32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foowf32( float32_t * pDataSrc, float32_t * pDataDest)
{
    const uint32x4_t vecOffs1 = { 0, 3, 6, 1};
    const uint32x4_t vecOffs2 = { 4, 7, 2, 5};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[8]);
    vstrwq_scatter_offset_f32 (pDataDest, vecOffs1, (float32x4_t) vecIn1);
    vstrwq_scatter_offset_f32 (pDataDest, vecOffs2, (float32x4_t) vecIn2);
    pDataDest[8] = pDataSrc[8];
    return 0;
}

int
foowu64( uint64_t * pDataSrc, uint64_t * pDataDest)
{
    const uint64x2_t vecOffs1 = { 0, 3};
    const uint64x2_t vecOffs2 = { 1, 2};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[2]);
    vstrdq_scatter_offset_u64 (pDataDest, vecOffs1, (uint64x2_t) vecIn1);
    vstrdq_scatter_offset_u64 (pDataDest, vecOffs2, (uint64x2_t) vecIn2);
    pDataDest[4] = pDataSrc[4];
    return 0;
}

int
foows64( int64_t * pDataSrc, int64_t * pDataDest)
{
    const uint64x2_t vecOffs1 = { 0, 3};
    const uint64x2_t vecOffs2 = { 1, 2};
    uint32x4_t vecIn1 = vldrwq_u32 ((uint32_t const *) pDataSrc);
    uint32x4_t vecIn2 = vldrwq_u32 ((uint32_t const *) &pDataSrc[2]);
    vstrdq_scatter_offset_s64 (pDataDest, vecOffs1, (int64x2_t) vecIn1);
    vstrdq_scatter_offset_s64 (pDataDest, vecOffs2, (int64x2_t) vecIn2);
    pDataDest[4] = pDataSrc[4];
    return 0;
}

/* { dg-final { scan-assembler-times "vstr\[a-z\]" 32 } } */
