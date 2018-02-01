/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

#define DO_CONSTANT(VALUE, TYPE, OP, NAME)			\
void vlogical_imm_##NAME##_##TYPE (TYPE *dst, int count)	\
{								\
  for (int i = 0; i < count; i++)				\
    dst[i] = dst[i] OP VALUE;					\
}

#define DO_LOGICAL_OPS_BRIEF(TYPE, OP, NAME)	\
  DO_CONSTANT (1, TYPE, OP, NAME ## 1)		\
  DO_CONSTANT (2, TYPE, OP, NAME ## 2)		\
  DO_CONSTANT (5, TYPE, OP, NAME ## 5)		\
  DO_CONSTANT (6, TYPE, OP, NAME ## 6)		\
  DO_CONSTANT (8, TYPE, OP, NAME ## 8)		\
  DO_CONSTANT (9, TYPE, OP, NAME ## 9)		\
  DO_CONSTANT (-1, TYPE, OP, NAME ## minus1)	\
  DO_CONSTANT (-2, TYPE, OP, NAME ## minus2)	\
  DO_CONSTANT (-5, TYPE, OP, NAME ## minus5)	\
  DO_CONSTANT (-6, TYPE, OP, NAME ## minus6)

#define DO_LOGICAL_OPS(TYPE, OP, NAME)				\
  DO_CONSTANT (1, TYPE, OP, NAME ## 1)				\
  DO_CONSTANT (2, TYPE, OP, NAME ## 2)				\
  DO_CONSTANT (3, TYPE, OP, NAME ## 3)				\
  DO_CONSTANT (4, TYPE, OP, NAME ## 4)				\
  DO_CONSTANT (5, TYPE, OP, NAME ## 5)				\
  DO_CONSTANT (6, TYPE, OP, NAME ## 6)				\
  DO_CONSTANT (7, TYPE, OP, NAME ## 7)				\
  DO_CONSTANT (8, TYPE, OP, NAME ## 8)				\
  DO_CONSTANT (9, TYPE, OP, NAME ## 9)				\
  DO_CONSTANT (10, TYPE, OP, NAME ## 10)			\
  DO_CONSTANT (11, TYPE, OP, NAME ## 11)			\
  DO_CONSTANT (12, TYPE, OP, NAME ## 12)			\
  DO_CONSTANT (13, TYPE, OP, NAME ## 13)			\
  DO_CONSTANT (14, TYPE, OP, NAME ## 14)			\
  DO_CONSTANT (15, TYPE, OP, NAME ## 15)			\
  DO_CONSTANT (16, TYPE, OP, NAME ## 16)			\
  DO_CONSTANT (17, TYPE, OP, NAME ## 17)			\
  DO_CONSTANT (18, TYPE, OP, NAME ## 18)			\
  DO_CONSTANT (19, TYPE, OP, NAME ## 19)			\
  DO_CONSTANT (20, TYPE, OP, NAME ## 20)			\
  DO_CONSTANT (21, TYPE, OP, NAME ## 21)			\
  DO_CONSTANT (22, TYPE, OP, NAME ## 22)			\
  DO_CONSTANT (23, TYPE, OP, NAME ## 23)			\
  DO_CONSTANT (24, TYPE, OP, NAME ## 24)			\
  DO_CONSTANT (25, TYPE, OP, NAME ## 25)			\
  DO_CONSTANT (26, TYPE, OP, NAME ## 26)			\
  DO_CONSTANT (27, TYPE, OP, NAME ## 27)			\
  DO_CONSTANT (28, TYPE, OP, NAME ## 28)			\
  DO_CONSTANT (29, TYPE, OP, NAME ## 29)			\
  DO_CONSTANT (30, TYPE, OP, NAME ## 30)			\
  DO_CONSTANT (31, TYPE, OP, NAME ## 31)			\
  DO_CONSTANT (32, TYPE, OP, NAME ## 32)			\
  DO_CONSTANT (33, TYPE, OP, NAME ## 33)			\
  DO_CONSTANT (34, TYPE, OP, NAME ## 34)			\
  DO_CONSTANT (35, TYPE, OP, NAME ## 35)			\
  DO_CONSTANT (252, TYPE, OP, NAME ## 252)			\
  DO_CONSTANT (253, TYPE, OP, NAME ## 253)			\
  DO_CONSTANT (254, TYPE, OP, NAME ## 254)			\
  DO_CONSTANT (255, TYPE, OP, NAME ## 255)			\
  DO_CONSTANT (256, TYPE, OP, NAME ## 256)			\
  DO_CONSTANT (257, TYPE, OP, NAME ## 257)			\
  DO_CONSTANT (65535, TYPE, OP, NAME ## 65535)			\
  DO_CONSTANT (65536, TYPE, OP, NAME ## 65536)			\
  DO_CONSTANT (65537, TYPE, OP, NAME ## 65537)			\
  DO_CONSTANT (2147483646, TYPE, OP, NAME ## 2147483646)	\
  DO_CONSTANT (2147483647, TYPE, OP, NAME ## 2147483647)	\
  DO_CONSTANT (2147483648, TYPE, OP, NAME ## 2147483648)	\
  DO_CONSTANT (-1, TYPE, OP, NAME ## minus1)			\
  DO_CONSTANT (-2, TYPE, OP, NAME ## minus2)			\
  DO_CONSTANT (-3, TYPE, OP, NAME ## minus3)			\
  DO_CONSTANT (-4, TYPE, OP, NAME ## minus4)			\
  DO_CONSTANT (-5, TYPE, OP, NAME ## minus5)			\
  DO_CONSTANT (-6, TYPE, OP, NAME ## minus6)			\
  DO_CONSTANT (-7, TYPE, OP, NAME ## minus7)			\
  DO_CONSTANT (-8, TYPE, OP, NAME ## minus8)			\
  DO_CONSTANT (-9, TYPE, OP, NAME ## minus9)

DO_LOGICAL_OPS_BRIEF (int8_t, &, and)
DO_LOGICAL_OPS_BRIEF (int64_t, &, and)

DO_LOGICAL_OPS (int32_t, &, and)
DO_LOGICAL_OPS (int32_t, |, or)
DO_LOGICAL_OPS (int32_t, ^, xor)

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x4\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x5\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0x6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0x8\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x9\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xa\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xb\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xc\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xd\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xe\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xf\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x10\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x11\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x12\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x13\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x14\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x15\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x16\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x17\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x18\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x19\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1a\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1b\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1c\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1d\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1e\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x1f\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x20\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x21\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x22\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x23\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfc\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfd\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfe\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xff\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x100\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x101\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x10000\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x10001\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x7ffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x7fffffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x80000000\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xffffffff\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0xfe\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffd\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffc\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffb\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffb\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0xfb\n} 1 } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffa\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffa\n} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.b, z[0-9]+\.b, #0xfa\n} } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff9\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff7\n} 1 } } */

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 28 } } */

/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x3\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x4\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x5\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x6\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x7\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x8\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x9\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xa\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xb\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xc\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xd\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xe\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xf\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x10\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x11\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x12\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x13\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x14\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x15\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x16\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x17\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x18\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x19\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1a\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1b\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1c\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1d\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1e\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x1f\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x20\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x21\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x22\n} } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x23\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfc\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfd\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfe\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xff\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x100\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x101\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x10000\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x10001\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x7ffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x7fffffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0x80000000\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xffffffff\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffd\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffc\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffb\n} 1 } } */
/* { dg-final { scan-assembler-not {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffa\n} } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff9\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff8\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff7\n} 1 } } */

/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 22 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x3\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x4\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x5\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x6\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x7\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x8\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x9\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xa\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xb\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xc\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xd\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xe\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xf\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x10\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x11\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x12\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x13\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x14\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x15\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x16\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x17\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x18\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x19\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1a\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1b\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1c\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1d\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1e\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x1f\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x20\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x21\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x22\n} } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x23\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfc\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfd\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfe\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xff\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x100\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x101\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x10000\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x10001\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x7ffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x7fffffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0x80000000\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xffffffff\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffe\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffd\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffc\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffb\n} 1 } } */
/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffffa\n} } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff9\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff8\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, z[0-9]+\.s, #0xfffffff7\n} 1 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 22 } } */
