/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=512 --save-temps" } */

#include <stdint.h>

typedef int64_t vnx4di __attribute__((vector_size (64)));
typedef int32_t vnx8si __attribute__((vector_size (64)));
typedef int16_t vnx16hi __attribute__((vector_size (64)));
typedef int8_t vnx32qi __attribute__((vector_size (64)));
typedef double vnx4df __attribute__((vector_size (64)));
typedef float vnx8sf __attribute__((vector_size (64)));
typedef _Float16 vnx16hf __attribute__((vector_size (64)));

#define EXTRACT(ELT_TYPE, TYPE, INDEX)		\
  ELT_TYPE permute_##TYPE##_##INDEX (void)	\
  {						\
    TYPE values;				\
    asm ("" : "=w" (values));			\
    return values[INDEX];			\
  }

#define TEST_ALL(T)				\
  T (int64_t, vnx4di, 0)			\
  T (int64_t, vnx4di, 1)			\
  T (int64_t, vnx4di, 2)			\
  T (int64_t, vnx4di, 7)			\
  T (int32_t, vnx8si, 0)			\
  T (int32_t, vnx8si, 1)			\
  T (int32_t, vnx8si, 3)			\
  T (int32_t, vnx8si, 4)			\
  T (int32_t, vnx8si, 15)			\
  T (int16_t, vnx16hi, 0)			\
  T (int16_t, vnx16hi, 1)			\
  T (int16_t, vnx16hi, 7)			\
  T (int16_t, vnx16hi, 8)			\
  T (int16_t, vnx16hi, 31)			\
  T (int8_t, vnx32qi, 0)			\
  T (int8_t, vnx32qi, 1)			\
  T (int8_t, vnx32qi, 15)			\
  T (int8_t, vnx32qi, 16)			\
  T (int8_t, vnx32qi, 63)			\
  T (double, vnx4df, 0)				\
  T (double, vnx4df, 1)				\
  T (double, vnx4df, 2)				\
  T (double, vnx4df, 7)				\
  T (float, vnx8sf, 0)				\
  T (float, vnx8sf, 1)				\
  T (float, vnx8sf, 3)				\
  T (float, vnx8sf, 4)				\
  T (float, vnx8sf, 15)				\
  T (_Float16, vnx16hf, 0)			\
  T (_Float16, vnx16hf, 1)			\
  T (_Float16, vnx16hf, 7)			\
  T (_Float16, vnx16hf, 8)			\
  T (_Float16, vnx16hf, 31)

TEST_ALL (EXTRACT)

/* { dg-final { scan-assembler-times {\tfmov\tx[0-9]+, d[0-9]+\n} 3 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d\[0\]\n} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\td[0-9]+, v[0-9]+\.d\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\td[0-9]+, v[0-9]+\.d\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.d, z[0-9]+\.d\[2\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.d, z[0-9]+\.d\[7\]\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfmov\tw[0-9]+, s[0-9]+\n} 3 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[0\]\n} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[3\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\ts[0-9]+, v[0-9]+\.s\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\ts[0-9]+, v[0-9]+\.s\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\ts[0-9]+, v[0-9]+\.s\[3\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.s, z[0-9]+\.s\[4\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.s, z[0-9]+\.s\[15\]\n} 2 } } */

/* Also used to move the result of a non-Advanced SIMD extract.  */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[0\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[7\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\th[0-9]+, v[0-9]+\.h\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\th[0-9]+, v[0-9]+\.h\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\th[0-9]+, v[0-9]+\.h\[7\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.h, z[0-9]+\.h\[8\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.h, z[0-9]+\.h\[31\]\n} 2 } } */

/* Also used to move the result of a non-Advanced SIMD extract.  */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[0\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[15\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.b, z[0-9]+\.b\[16\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.b, z[0-9]+\.b\[63\]\n} 1 } } */
