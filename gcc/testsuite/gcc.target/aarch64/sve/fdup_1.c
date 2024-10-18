/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* -fno-tree-loop-distribute-patterns prevents conversion to memset.  */
/* { dg-options "-O3 -fno-tree-loop-distribute-patterns --save-temps" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (1024 / sizeof (TYPE))

#define DEF_SET_IMM(TYPE, IMM, SUFFIX)		\
void __attribute__ ((noinline, noclone))	\
set_##TYPE##_##SUFFIX (TYPE *a)			\
{						\
  for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
    a[i] = IMM;					\
}

#define DEF_SET_IMM_FP(IMM, SUFFIX) \
  DEF_SET_IMM (float, IMM, SUFFIX)  \
  DEF_SET_IMM (double, IMM, SUFFIX)

/* Valid.  */
DEF_SET_IMM_FP (1, imm1)
DEF_SET_IMM_FP (0x1.1p0, imm1p0)
DEF_SET_IMM_FP (0x1.fp0, immfp0)
DEF_SET_IMM_FP (0x1.1p4, imm1p4)
DEF_SET_IMM_FP (0x1.1p-3, imm1pm3)
DEF_SET_IMM_FP (0x1.fp4, immfp4)
DEF_SET_IMM_FP (0x1.fp-3, immfpm3)

/* Should use MOV instead.  */
DEF_SET_IMM_FP (0, imm0)

/* Invalid.  */
DEF_SET_IMM_FP (0x1.1fp0, imm1fp0)
DEF_SET_IMM_FP (0x1.1p5, imm1p5)
DEF_SET_IMM_FP (0x1.1p-4, imm1pm4)
DEF_SET_IMM_FP (0x1.1fp5, imm1fp5)
DEF_SET_IMM_FP (0x1.1fp-4, imm1fpm4)

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s,} 7 } } */

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #1.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #1.0625e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #1.9375e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #1.7e\+1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #1.328125e-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #3.1e\+1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #2.421875e-1\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d,} 7 } } */

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #1.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #1.0625e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #1.9375e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #1.7e\+1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #1.328125e-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #3.1e\+1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #2.421875e-1\n} 1 } } */

/* { dg-final { scan-assembler-times {\tmov(?:i\td|\tz)([0-9]+)(?:\.[bhsd])?, #0\n} 2 } } */
