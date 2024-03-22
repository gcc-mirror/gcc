/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl32b_zfh_zvfh -mabi=lp64 -mrvv-vector-bits=zvl -O3" } */

#include "riscv_rvv_vector_bits.h"

TEST_FIXED_TYPE_INT_ALL (vint8mf4_t,  8,  v1qi)
TEST_FIXED_TYPE_INT_ALL (vint8mf2_t, 16,  v2qi)
TEST_FIXED_TYPE_INT_ALL (vint8m1_t,  32,  v4qi)
TEST_FIXED_TYPE_INT_ALL (vint8m2_t,  64,  v8qi)
TEST_FIXED_TYPE_INT_ALL (vint8m4_t, 128, v16qi)
TEST_FIXED_TYPE_INT_ALL (vint8m8_t, 256, v32qi)

TEST_FIXED_TYPE_INT_ALL (vint16mf2_t,  16,  v1hi)
TEST_FIXED_TYPE_INT_ALL (vint16m1_t,   32,  v2hi)
TEST_FIXED_TYPE_INT_ALL (vint16m2_t,   64,  v4hi)
TEST_FIXED_TYPE_INT_ALL (vint16m4_t,  128,  v8hi)
TEST_FIXED_TYPE_INT_ALL (vint16m8_t,  256, v16hi)

TEST_FIXED_TYPE_INT_ALL (vint32m1_t,  32, v1si)
TEST_FIXED_TYPE_INT_ALL (vint32m2_t,  64, v2si)
TEST_FIXED_TYPE_INT_ALL (vint32m4_t, 128, v4si)
TEST_FIXED_TYPE_INT_ALL (vint32m8_t, 256, v8si)

TEST_FIXED_TYPE_INT_ALL (vuint8mf4_t,  8,  v1uqi)
TEST_FIXED_TYPE_INT_ALL (vuint8mf2_t, 16,  v2uqi)
TEST_FIXED_TYPE_INT_ALL (vuint8m1_t,  32,  v4uqi)
TEST_FIXED_TYPE_INT_ALL (vuint8m2_t,  64,  v8uqi)
TEST_FIXED_TYPE_INT_ALL (vuint8m4_t, 128, v16uqi)
TEST_FIXED_TYPE_INT_ALL (vuint8m8_t, 256, v32uqi)

TEST_FIXED_TYPE_INT_ALL (vuint16mf2_t,  16,  v1uhi)
TEST_FIXED_TYPE_INT_ALL (vuint16m1_t,   32,  v2uhi)
TEST_FIXED_TYPE_INT_ALL (vuint16m2_t,   64,  v4uhi)
TEST_FIXED_TYPE_INT_ALL (vuint16m4_t,  128,  v8uhi)
TEST_FIXED_TYPE_INT_ALL (vuint16m8_t,  256, v16uhi)

TEST_FIXED_TYPE_INT_ALL (vuint32m1_t,  32, v1usi)
TEST_FIXED_TYPE_INT_ALL (vuint32m2_t,  64, v2usi)
TEST_FIXED_TYPE_INT_ALL (vuint32m4_t, 128, v4usi)
TEST_FIXED_TYPE_INT_ALL (vuint32m8_t, 256, v8usi)

TEST_FIXED_TYPE_FLOAT_ALL (vfloat16mf2_t,  16,  v1hf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat16m1_t,   32,  v2hf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat16m2_t,   64,  v4hf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat16m4_t,  128,  v8hf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat16m8_t,  256, v16hf)

TEST_FIXED_TYPE_FLOAT_ALL (vfloat32m1_t,  32, v1sf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat32m2_t,  64, v2sf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat32m4_t, 128, v4sf)
TEST_FIXED_TYPE_FLOAT_ALL (vfloat32m8_t, 256, v8sf)

/* { dg-final { scan-assembler-not {csrr\s+[atx][0-9]+,\s*vlenb} } } */
