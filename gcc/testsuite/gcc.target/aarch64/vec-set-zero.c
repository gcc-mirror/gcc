/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "arm_neon.h"

#define FOO(type) \
type f_##type(type v) \
{ \
  v[1] = 0; \
  return v; \
}

FOO(int8x8_t)
FOO(int16x4_t)
FOO(int32x2_t)

FOO(int8x16_t)
FOO(int16x8_t)
FOO(int32x4_t)
FOO(int64x2_t)

FOO(float16x4_t)
FOO(float32x2_t)

FOO(float16x8_t)
FOO(float32x4_t)
FOO(float64x2_t)

/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.b\[1\], wzr} 2 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.h\[1\], wzr} 4 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.s\[1\], wzr} 4 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.d\[1\], xzr} 2 { target aarch64_little_endian } } } */

/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.b\[6\], wzr} 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.b\[14\], wzr} 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.h\[2\], wzr} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.h\[6\], wzr} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.s\[0\], wzr} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.s\[2\], wzr} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {ins\tv[0-9]+\.d\[0\], xzr} 2 { target aarch64_big_endian } } } */
