/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int8_t vnx16qi __attribute__((vector_size(32)));
typedef int16_t vnx8hi __attribute__((vector_size(32)));
typedef int32_t vnx4si __attribute__((vector_size(32)));
typedef int64_t vnx2di __attribute__((vector_size(32)));

#define DO_OP(TYPE)						\
void vmla_##TYPE (TYPE *x, TYPE y, TYPE z)			\
{								\
  register TYPE dst  asm("z0");					\
  register TYPE src1 asm("z2");					\
  register TYPE src2 asm("z4");					\
  dst = *x;							\
  src1 = y;							\
  src2 = z;							\
  asm volatile ("" :: "w" (dst), "w" (src1), "w" (src2));	\
  dst = (src1 * src2) + dst;					\
  asm volatile ("" :: "w" (dst));				\
  *x = dst;							\
}

DO_OP (vnx16qi)
DO_OP (vnx8hi)
DO_OP (vnx4si)
DO_OP (vnx2di)

/* { dg-final { scan-assembler-times {\tmla\tz0\.b, p[0-7]/m, z2\.b, z4\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmla\tz0\.h, p[0-7]/m, z2\.h, z4\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmla\tz0\.s, p[0-7]/m, z2\.s, z4\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmla\tz0\.d, p[0-7]/m, z2\.d, z4\.d\n} 1 } } */
