/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+lse" } */

/* Test ARMv8.1-A Load-ADD instruction.  */

#include "atomic-inst-ops.inc"

#define TEST TEST_ONE

#define LOAD_ADD(FN, TY, MODEL)						\
  TY FNNAME (FN, TY) (TY* val, TY* foo)					\
  {									\
    return __atomic_fetch_add (val, foo, MODEL);			\
  }

#define LOAD_ADD_NORETURN(FN, TY, MODEL)				\
  void FNNAME (FN, TY) (TY* val, TY* foo)				\
  {									\
    __atomic_fetch_add (val, foo, MODEL);				\
  }

#define LOAD_SUB(FN, TY, MODEL)						\
  TY FNNAME (FN, TY) (TY* val, TY* foo)					\
  {									\
    return __atomic_fetch_sub (val, foo, MODEL);			\
  }

#define LOAD_SUB_NORETURN(FN, TY, MODEL)				\
  void FNNAME (FN, TY) (TY* val, TY* foo)				\
  {									\
    __atomic_fetch_sub (val, foo, MODEL);				\
  }


TEST (load_add, LOAD_ADD)
TEST (load_add_notreturn, LOAD_ADD_NORETURN)

TEST (load_sub, LOAD_SUB)
TEST (load_sub_notreturn, LOAD_SUB_NORETURN)

/* { dg-final { scan-assembler-times "ldaddb\t" 8} } */
/* { dg-final { scan-assembler-times "ldaddab\t" 16} } */
/* { dg-final { scan-assembler-times "ldaddlb\t" 8} } */
/* { dg-final { scan-assembler-times "ldaddalb\t" 16} } */

/* { dg-final { scan-assembler-times "ldaddh\t" 8} } */
/* { dg-final { scan-assembler-times "ldaddah\t" 16} } */
/* { dg-final { scan-assembler-times "ldaddlh\t" 8} } */
/* { dg-final { scan-assembler-times "ldaddalh\t" 16} } */

/* { dg-final { scan-assembler-times "ldadd\t" 16} } */
/* { dg-final { scan-assembler-times "ldadda\t" 32} } */
/* { dg-final { scan-assembler-times "ldaddl\t" 16} } */
/* { dg-final { scan-assembler-times "ldaddal\t" 32} } */

/* { dg-final { scan-assembler-not "ldaxr\t" } } */
/* { dg-final { scan-assembler-not "stlxr\t" } } */
/* { dg-final { scan-assembler-not "dmb" } } */
