/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O3 -fno-tree-loop-distribute-patterns" } */

#include "fdup_1.c"

#define TEST_SET_IMM(TYPE,IMM,SUFFIX)		\
  {						\
    TYPE v[NUM_ELEMS (TYPE)];			\
    set_##TYPE##_##SUFFIX (v);			\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++ )	\
      if (v[i] != IMM)				\
	__builtin_abort ();			\
  }

#define TEST_SET_IMM_FP(IMM, SUFFIX) \
  TEST_SET_IMM (float, IMM, SUFFIX)  \
  TEST_SET_IMM (double, IMM, SUFFIX)

int __attribute__ ((optimize (1)))
main (int argc, char **argv)
{
  TEST_SET_IMM_FP (1, imm1)
  TEST_SET_IMM_FP (0x1.1p0, imm1p0)
  TEST_SET_IMM_FP (0x1.fp0, immfp0)
  TEST_SET_IMM_FP (0x1.1p4, imm1p4)
  TEST_SET_IMM_FP (0x1.1p-3, imm1pm3)
  TEST_SET_IMM_FP (0x1.fp4, immfp4)
  TEST_SET_IMM_FP (0x1.fp-3, immfpm3)

  TEST_SET_IMM_FP (0, imm0)
  TEST_SET_IMM_FP (0x1.1fp0, imm1fp0)
  TEST_SET_IMM_FP (0x1.1p5, imm1p5)
  TEST_SET_IMM_FP (0x1.1p-4, imm1pm4)
  TEST_SET_IMM_FP (0x1.1fp5, imm1fp5)
  TEST_SET_IMM_FP (0x1.1fp-4, imm1fpm4)

  return 0;
}
