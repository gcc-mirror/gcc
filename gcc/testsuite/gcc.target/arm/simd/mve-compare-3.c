/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations" } */

/* float 16 tests.  */

#ifndef ELEM_TYPE
#define ELEM_TYPE __fp16
#endif
#ifndef INT_ELEM_TYPE
#define INT_ELEM_TYPE __INT16_TYPE__
#endif

#define COMPARE(NAME, OP)			\
  int_vec					\
  cmp_##NAME##_reg (vec a, vec b)		\
  {						\
    return a OP b;				\
  }

typedef INT_ELEM_TYPE int_vec __attribute__((vector_size(16)));
typedef ELEM_TYPE vec __attribute__((vector_size(16)));

COMPARE (eq, ==)
COMPARE (ne, !=)
COMPARE (lt, <)
COMPARE (le, <=)
COMPARE (gt, >)
COMPARE (ge, >=)

/* eq, ne, lt, le, gt, ge.
/* { dg-final { scan-assembler-times {\tvcmp.f16\teq, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tne, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tlt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tle, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tgt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.f16\tge, q[0-9]+, q[0-9]+\n} 1 } } */
