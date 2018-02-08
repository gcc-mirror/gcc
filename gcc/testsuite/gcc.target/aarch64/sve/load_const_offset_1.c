/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int64_t vnx2di __attribute__ ((vector_size (32)));
typedef int32_t vnx4si __attribute__ ((vector_size (32)));
typedef int16_t vnx8hi __attribute__ ((vector_size (32)));
typedef int8_t vnx16qi __attribute__ ((vector_size (32)));

#define TEST_TYPE(TYPE)						\
  void sve_load_##TYPE##_neg9 (TYPE *a)				\
  {								\
    register TYPE x asm ("z0") = a[-9];				\
    asm volatile ("" :: "w" (x));				\
  }								\
								\
  void sve_load_##TYPE##_neg8 (TYPE *a)				\
  {								\
    register TYPE x asm ("z0") = a[-8];				\
    asm volatile ("" :: "w" (x));				\
  }								\
								\
  void sve_load_##TYPE##_0 (TYPE *a)				\
  {								\
    register TYPE x asm ("z0") = a[0];				\
    asm volatile ("" :: "w" (x));				\
  }								\
								\
  void sve_load_##TYPE##_unaligned (TYPE *a)			\
  {								\
    register TYPE x asm ("z0") = *(TYPE *) ((char *) a + 16);	\
    asm volatile ("" :: "w" (x));				\
  }								\
								\
  void sve_load_##TYPE##_7 (TYPE *a)				\
  {								\
    register TYPE x asm ("z0") = a[7];				\
    asm volatile ("" :: "w" (x));				\
  }								\
								\
  void sve_load_##TYPE##_8 (TYPE *a)				\
  {								\
    register TYPE x asm ("z0") = a[8];				\
    asm volatile ("" :: "w" (x));				\
  }

TEST_TYPE (vnx2di)
TEST_TYPE (vnx4si)
TEST_TYPE (vnx8hi)
TEST_TYPE (vnx16qi)

/* { dg-final { scan-assembler-times {\tsub\tx[0-9]+, x0, #288\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tx[0-9]+, x0, 16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tx[0-9]+, x0, 256\n} 4 } } */

/* { dg-final { scan-assembler-not {\tld1d\tz0\.d, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler-times {\tld1d\tz0\.d, p[0-7]/z, \[x0, #-8, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz0\.d, p[0-7]/z, \[x0\]\n} 4 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz0\.d, p[0-7]/z, \[x0, #7, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tld1d\tz0\.d, p[0-7]/z, \[x0, #8, mul vl\]\n} } } */

/* { dg-final { scan-assembler-not {\tld1w\tz0\.s, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler-times {\tld1w\tz0\.s, p[0-7]/z, \[x0, #-8, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz0\.s, p[0-7]/z, \[x0\]\n} 4 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz0\.s, p[0-7]/z, \[x0, #7, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tld1w\tz0\.s, p[0-7]/z, \[x0, #8, mul vl\]\n} } } */

/* { dg-final { scan-assembler-not {\tld1h\tz0\.h, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler-times {\tld1h\tz0\.h, p[0-7]/z, \[x0, #-8, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz0\.h, p[0-7]/z, \[x0\]\n} 4 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz0\.h, p[0-7]/z, \[x0, #7, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tld1h\tz0\.h, p[0-7]/z, \[x0, #8, mul vl\]\n} } } */

/* { dg-final { scan-assembler-not {\tld1b\tz0\.b, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler-times {\tld1b\tz0\.b, p[0-7]/z, \[x0, #-8, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz0\.b, p[0-7]/z, \[x0\]\n} 4 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz0\.b, p[0-7]/z, \[x0, #7, mul vl\]\n} 1 } } */
/* { dg-final { scan-assembler-not {\tld1b\tz0\.b, p[0-7]/z, \[x0, #8, mul vl\]\n} } } */
