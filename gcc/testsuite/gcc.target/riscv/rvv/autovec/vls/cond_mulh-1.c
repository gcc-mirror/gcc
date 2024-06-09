/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_MULH (cond_mulh, 4, v4qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 8, v8qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 16, v16qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 32, v32qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 64, v64qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 128, v128qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 256, v256qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 512, v512qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 1024, v1024qi, int16_t, int8_t, 8)
DEF_COND_MULH (cond_mulh, 2048, v2048qi, int16_t, int8_t, 8)

DEF_COND_MULH (cond_mulh, 4, v4hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 8, v8hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 16, v16hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 32, v32hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 64, v64hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 128, v128hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 256, v256hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 512, v512hi, int32_t, int16_t, 16)
DEF_COND_MULH (cond_mulh, 1024, v1024hi, int32_t, int16_t, 16)

DEF_COND_MULH (cond_mulh, 4, v4si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 8, v8si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 16, v16si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 32, v32si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 64, v64si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 128, v128si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 256, v256si, int64_t, int32_t, 32)
DEF_COND_MULH (cond_mulh, 512, v512si, int64_t, int32_t, 32)

DEF_COND_MULH (cond_mulh, 4, v4uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 8, v8uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 16, v16uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 32, v32uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 64, v64uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 128, v128uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 256, v256uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 512, v512uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 1024, v1024uqi, uint16_t, uint8_t, 8)
DEF_COND_MULH (cond_mulh, 2048, v2048uqi, uint16_t, uint8_t, 8)

DEF_COND_MULH (cond_mulh, 4, v4uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 8, v8uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 16, v16uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 32, v32uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 64, v64uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 128, v128uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 256, v256uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 512, v512uhi, uint32_t, uint16_t, 16)
DEF_COND_MULH (cond_mulh, 1024, v1024uhi, uint32_t, uint16_t, 16)

DEF_COND_MULH (cond_mulh, 4, v4usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 8, v8usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 16, v16usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 32, v32usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 64, v64usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 128, v128usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 256, v256usi, uint64_t, uint32_t, 32)
DEF_COND_MULH (cond_mulh, 512, v512usi, uint64_t, uint32_t, 32)

/* { dg-final { scan-assembler-times {vmulhu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 27 } } */
/* { dg-final { scan-assembler-times {vmulh\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 27 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {vmerge} } } */
/* { dg-final { scan-tree-dump-not "1,1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2,2" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4,4" "optimized" } } */
/* { dg-final { scan-tree-dump-not "16,16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "32,32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64,64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "128,128" "optimized" } } */
/* { dg-final { scan-tree-dump-not "256,256" "optimized" } } */
/* { dg-final { scan-tree-dump-not "512,512" "optimized" } } */
/* { dg-final { scan-tree-dump-not "1024,1024" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2048,2048" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4096,4096" "optimized" } } */
