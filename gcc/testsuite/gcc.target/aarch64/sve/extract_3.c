/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=1024 --save-temps" } */

#include <stdint.h>

typedef int64_t vnx8di __attribute__((vector_size (128)));
typedef int32_t vnx16si __attribute__((vector_size (128)));
typedef int16_t vnx32hi __attribute__((vector_size (128)));
typedef int8_t vnx64qi __attribute__((vector_size (128)));
typedef double vnx8df __attribute__((vector_size (128)));
typedef float vnx16sf __attribute__((vector_size (128)));
typedef _Float16 vnx32hf __attribute__((vector_size (128)));

#define EXTRACT(ELT_TYPE, TYPE, INDEX)		\
  ELT_TYPE permute_##TYPE##_##INDEX (void)	\
  {						\
    TYPE values;				\
    asm ("" : "=w" (values));			\
    return values[INDEX];			\
  }

#define TEST_ALL(T)				\
  T (int64_t, vnx8di, 0)			\
  T (int64_t, vnx8di, 1)			\
  T (int64_t, vnx8di, 2)			\
  T (int64_t, vnx8di, 7)			\
  T (int64_t, vnx8di, 8)			\
  T (int64_t, vnx8di, 9)			\
  T (int64_t, vnx8di, 15)			\
  T (int32_t, vnx16si, 0)			\
  T (int32_t, vnx16si, 1)			\
  T (int32_t, vnx16si, 3)			\
  T (int32_t, vnx16si, 4)			\
  T (int32_t, vnx16si, 15)			\
  T (int32_t, vnx16si, 16)			\
  T (int32_t, vnx16si, 21)			\
  T (int32_t, vnx16si, 31)			\
  T (int16_t, vnx32hi, 0)			\
  T (int16_t, vnx32hi, 1)			\
  T (int16_t, vnx32hi, 7)			\
  T (int16_t, vnx32hi, 8)			\
  T (int16_t, vnx32hi, 31)			\
  T (int16_t, vnx32hi, 32)			\
  T (int16_t, vnx32hi, 47)			\
  T (int16_t, vnx32hi, 63)			\
  T (int8_t, vnx64qi, 0)			\
  T (int8_t, vnx64qi, 1)			\
  T (int8_t, vnx64qi, 15)			\
  T (int8_t, vnx64qi, 16)			\
  T (int8_t, vnx64qi, 63)			\
  T (int8_t, vnx64qi, 64)			\
  T (int8_t, vnx64qi, 100)			\
  T (int8_t, vnx64qi, 127)			\
  T (double, vnx8df, 0)				\
  T (double, vnx8df, 1)				\
  T (double, vnx8df, 2)				\
  T (double, vnx8df, 7)				\
  T (double, vnx8df, 8)				\
  T (double, vnx8df, 9)				\
  T (double, vnx8df, 15)			\
  T (float, vnx16sf, 0)				\
  T (float, vnx16sf, 1)				\
  T (float, vnx16sf, 3)				\
  T (float, vnx16sf, 4)				\
  T (float, vnx16sf, 15)			\
  T (float, vnx16sf, 16)			\
  T (float, vnx16sf, 21)			\
  T (float, vnx16sf, 31)			\
  T (_Float16, vnx32hf, 0)			\
  T (_Float16, vnx32hf, 1)			\
  T (_Float16, vnx32hf, 7)			\
  T (_Float16, vnx32hf, 8)			\
  T (_Float16, vnx32hf, 31)			\
  T (_Float16, vnx32hf, 32)			\
  T (_Float16, vnx32hf, 47)			\
  T (_Float16, vnx32hf, 63)

TEST_ALL (EXTRACT)

/* { dg-final { scan-assembler-times {\tfmov\tx[0-9]+, d[0-9]+\n} 5 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d\[0\]\n} 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\td[0-9]+, v[0-9]+\.d\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\td[0-9]+, v[0-9]+\.d\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.d, z[0-9]+\.d\[2\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.d, z[0-9]+\.d\[7\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlastb\tx[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmov\tw[0-9]+, s[0-9]+\n} 5 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[0\]\n} 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[3\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\ts[0-9]+, v[0-9]+\.s\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\ts[0-9]+, v[0-9]+\.s\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\ts[0-9]+, v[0-9]+\.s\[3\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.s, z[0-9]+\.s\[4\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.s, z[0-9]+\.s\[15\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */

/* Also used to move the result of a non-Advanced SIMD extract.  */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[0\]\n} 5 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[7\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\th[0-9]+, v[0-9]+\.h\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\th[0-9]+, v[0-9]+\.h\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\th[0-9]+, v[0-9]+\.h\[7\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.h, z[0-9]+\.h\[8\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.h, z[0-9]+\.h\[31\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */

/* Also used to move the result of a non-Advanced SIMD extract.  */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[0\]\n} 5 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[15\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.b, z[0-9]+\.b\[16\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.b, z[0-9]+\.b\[63\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */

/* { dg-final { scan-assembler-times {\text\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b, #64\n} 7 } } */
/* { dg-final { scan-assembler-times {\text\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b, #72\n} 2 } } */
/* { dg-final { scan-assembler-times {\text\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b, #84\n} 2 } } */
/* { dg-final { scan-assembler-times {\text\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b, #94\n} 2 } } */
/* { dg-final { scan-assembler-times {\text\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b, #100\n} 1 } } */
