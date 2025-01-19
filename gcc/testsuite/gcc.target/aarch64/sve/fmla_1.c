/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-options "-O3 -msve-vector-bits=256 --save-temps" } */

typedef __bf16 vnx8bf __attribute__((vector_size(32)));
typedef _Float16 vnx8hf __attribute__((vector_size(32)));
typedef float vnx4sf __attribute__((vector_size(32)));
typedef double vnx2df __attribute__((vector_size(32)));

#define DO_OP(TYPE)						\
void vmad##TYPE (TYPE *x, TYPE y, TYPE z)			\
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

DO_OP (vnx8hf)
DO_OP (vnx4sf)
DO_OP (vnx2df)

#pragma GCC target "+sve2+sve-b16b16"

DO_OP (vnx8bf)

/* { dg-final { scan-assembler-times {\tbfmla\tz0\.h, p[0-7]/m, z2\.h, z4\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmla\tz0\.h, p[0-7]/m, z2\.h, z4\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmla\tz0\.s, p[0-7]/m, z2\.s, z4\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmla\tz0\.d, p[0-7]/m, z2\.d, z4\.d\n} 1 } } */
