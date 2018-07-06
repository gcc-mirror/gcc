/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -msve-vector-bits=256 --save-temps" } */

typedef _Float16 vnx8hf __attribute__((vector_size(32)));
typedef float vnx4sf __attribute__((vector_size(32)));
typedef double vnx2df __attribute__((vector_size(32)));

#define DO_OP(TYPE)				\
void vdiv_##TYPE (TYPE *x, TYPE y)		\
{						\
  register TYPE dst asm("z0");			\
  register TYPE src asm("z2");			\
  dst = *x;					\
  src = y;					\
  asm volatile ("" :: "w" (dst), "w" (src));	\
  dst = dst / src;				\
  asm volatile ("" :: "w" (dst));		\
  *x = dst;					\
}						\
void vdivr_##TYPE (TYPE *x, TYPE y)		\
{						\
  register TYPE dst asm("z0");			\
  register TYPE src asm("z2");			\
  dst = *x;					\
  src = y;					\
  asm volatile ("" :: "w" (dst), "w" (src));	\
  dst = src / dst;				\
  asm volatile ("" :: "w" (dst));		\
  *x = dst;					\
}

DO_OP (vnx8hf)
DO_OP (vnx4sf)
DO_OP (vnx2df)

/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfdivr\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfdivr\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfdivr\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
