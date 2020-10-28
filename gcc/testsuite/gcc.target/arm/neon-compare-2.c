/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1 -funsafe-math-optimizations" }  */
/* { dg-add-options arm_neon } */

#ifndef ELEM_TYPE
#define ELEM_TYPE float
#endif
#ifndef INT_ELEM_TYPE
#define INT_ELEM_TYPE __INT32_TYPE__
#endif

#define COMPARE(NAME, OP) \
  int_vec \
  cmp_##NAME##_reg (vec a, vec b) \
  { \
    return a OP b; \
  } \
  \
  int_vec \
  cmp_##NAME##_zero (vec a) \
  { \
    return a OP (vec) {}; \
  }

typedef INT_ELEM_TYPE int_vec __attribute__((vector_size(16)));
typedef ELEM_TYPE vec __attribute__((vector_size(16)));

COMPARE (eq, ==)
COMPARE (ne, !=)
COMPARE (lt, <)
COMPARE (le, <=)
COMPARE (gt, >)
COMPARE (ge, >=)

/* { dg-final { scan-assembler-times {\tvceq.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvceq.f32\tq[0-9]+, q[0-9]+, #0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tvcgt.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.f32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvclt.f32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcge.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.f32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.f32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
