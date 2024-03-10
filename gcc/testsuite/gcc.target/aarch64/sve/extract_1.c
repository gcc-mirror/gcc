/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int64_t vnx2di __attribute__((vector_size (32)));
typedef int32_t vnx4si __attribute__((vector_size (32)));
typedef int16_t vnx8hi __attribute__((vector_size (32)));
typedef int8_t vnx16qi __attribute__((vector_size (32)));
typedef double vnx2df __attribute__((vector_size (32)));
typedef float vnx4sf __attribute__((vector_size (32)));
typedef _Float16 vnx8hf __attribute__((vector_size (32)));

#define EXTRACT(ELT_TYPE, TYPE, INDEX)		\
  ELT_TYPE permute_##TYPE##_##INDEX (void)	\
  {						\
    TYPE values;				\
    asm ("" : "=w" (values));			\
    return values[INDEX];			\
  }

#define TEST_ALL(T)				\
  T (int64_t, vnx2di, 0)			\
  T (int64_t, vnx2di, 1)			\
  T (int64_t, vnx2di, 2)			\
  T (int64_t, vnx2di, 3)			\
  T (int32_t, vnx4si, 0)			\
  T (int32_t, vnx4si, 1)			\
  T (int32_t, vnx4si, 3)			\
  T (int32_t, vnx4si, 4)			\
  T (int32_t, vnx4si, 7)			\
  T (int16_t, vnx8hi, 0)			\
  T (int16_t, vnx8hi, 1)			\
  T (int16_t, vnx8hi, 7)			\
  T (int16_t, vnx8hi, 8)			\
  T (int16_t, vnx8hi, 15)			\
  T (int8_t, vnx16qi, 0)			\
  T (int8_t, vnx16qi, 1)			\
  T (int8_t, vnx16qi, 15)			\
  T (int8_t, vnx16qi, 16)			\
  T (int8_t, vnx16qi, 31)			\
  T (double, vnx2df, 0)				\
  T (double, vnx2df, 1)				\
  T (double, vnx2df, 2)				\
  T (double, vnx2df, 3)				\
  T (float, vnx4sf, 0)				\
  T (float, vnx4sf, 1)				\
  T (float, vnx4sf, 3)				\
  T (float, vnx4sf, 4)				\
  T (float, vnx4sf, 7)				\
  T (_Float16, vnx8hf, 0)			\
  T (_Float16, vnx8hf, 1)			\
  T (_Float16, vnx8hf, 7)			\
  T (_Float16, vnx8hf, 8)			\
  T (_Float16, vnx8hf, 15)

TEST_ALL (EXTRACT)

/* { dg-final { scan-assembler-times {\tfmov\tx[0-9]+, d[0-9]+\n} 2 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d\[0\]\n} 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tx[0-9]+, v[0-9]+\.d\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\td[0-9]+, v[0-9]+\.d\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\td[0-9]+, v[0-9]+\.d\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.d, z[0-9]+\.d\[2\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlastb\tx[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmov\tw[0-9]+, s[0-9]+\n} 2 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[0\]\n} 1 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.s\[3\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\ts[0-9]+, v[0-9]+\.s\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\ts[0-9]+, v[0-9]+\.s\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\ts[0-9]+, v[0-9]+\.s\[3\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.s, z[0-9]+\.s\[4\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */

/* Also used to move the result of a non-Advanced SIMD extract.  */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[0\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.h\[7\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tdup\th[0-9]+, v[0-9]+\.h\[0\]\n} } } */
/* { dg-final { scan-assembler-times {\tdup\th[0-9]+, v[0-9]+\.h\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\th[0-9]+, v[0-9]+\.h\[7\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.h, z[0-9]+\.h\[8\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */

/* Also used to move the result of a non-Advanced SIMD extract.  */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[0\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[1\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumov\tw[0-9]+, v[0-9]+\.b\[15\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.b, z[0-9]+\.b\[16\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
