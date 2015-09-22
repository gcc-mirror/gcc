/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+lse" } */

/* Test ARMv8.1-A LD<logic-op> instruction.  */

#include "atomic-inst-ops.inc"

#define TEST TEST_ONE

#define LOAD_OR(FN, TY, MODEL)						\
  TY FNNAME (FN, TY) (TY* val, TY* foo)					\
  {									\
    return __atomic_fetch_or (val, foo, MODEL);				\
  }

#define LOAD_OR_NORETURN(FN, TY, MODEL)					\
  void FNNAME (FN, TY) (TY* val, TY* foo)				\
  {									\
    __atomic_fetch_or (val, foo, MODEL);				\
  }

#define LOAD_AND(FN, TY, MODEL)						\
  TY FNNAME (FN, TY) (TY* val, TY* foo)					\
  {									\
    return __atomic_fetch_and (val, foo, MODEL);			\
  }

#define LOAD_AND_NORETURN(FN, TY, MODEL)				\
  void FNNAME (FN, TY) (TY* val, TY* foo)				\
  {									\
    __atomic_fetch_and (val, foo, MODEL);				\
  }

#define LOAD_XOR(FN, TY, MODEL)						\
  TY FNNAME (FN, TY) (TY* val, TY* foo)					\
  {									\
    return __atomic_fetch_xor (val, foo, MODEL);			\
  }

#define LOAD_XOR_NORETURN(FN, TY, MODEL)				\
  void FNNAME (FN, TY) (TY* val, TY* foo)				\
  {									\
    __atomic_fetch_xor (val, foo, MODEL);				\
  }


TEST (load_or, LOAD_OR)
TEST (load_or_notreturn, LOAD_OR_NORETURN)

TEST (load_and, LOAD_AND)
TEST (load_and_notreturn, LOAD_AND_NORETURN)

TEST (load_xor, LOAD_XOR)
TEST (load_xor_notreturn, LOAD_XOR_NORETURN)

/* Load-OR.  */

/* { dg-final { scan-assembler-times "ldsetb\t" 4} } */
/* { dg-final { scan-assembler-times "ldsetab\t" 8} } */
/* { dg-final { scan-assembler-times "ldsetlb\t" 4} } */
/* { dg-final { scan-assembler-times "ldsetalb\t" 8} } */

/* { dg-final { scan-assembler-times "ldseth\t" 4} } */
/* { dg-final { scan-assembler-times "ldsetah\t" 8} } */
/* { dg-final { scan-assembler-times "ldsetlh\t" 4} } */
/* { dg-final { scan-assembler-times "ldsetalh\t" 8} } */

/* { dg-final { scan-assembler-times "ldset\t" 8} } */
/* { dg-final { scan-assembler-times "ldseta\t" 16} } */
/* { dg-final { scan-assembler-times "ldsetl\t" 8} } */
/* { dg-final { scan-assembler-times "ldsetal\t" 16} } */

/* Load-AND.  */

/* { dg-final { scan-assembler-times "ldclrb\t" 4} } */
/* { dg-final { scan-assembler-times "ldclrab\t" 8} } */
/* { dg-final { scan-assembler-times "ldclrlb\t" 4} } */
/* { dg-final { scan-assembler-times "ldclralb\t" 8} } */

/* { dg-final { scan-assembler-times "ldclrh\t" 4} } */
/* { dg-final { scan-assembler-times "ldclrah\t" 8} } */
/* { dg-final { scan-assembler-times "ldclrlh\t" 4} } */
/* { dg-final { scan-assembler-times "ldclralh\t" 8} } */

/* { dg-final { scan-assembler-times "ldclr\t" 8} */
/* { dg-final { scan-assembler-times "ldclra\t" 16} } */
/* { dg-final { scan-assembler-times "ldclrl\t" 8} } */
/* { dg-final { scan-assembler-times "ldclral\t" 16} } */

/* Load-XOR.  */

/* { dg-final { scan-assembler-times "ldeorb\t" 4} } */
/* { dg-final { scan-assembler-times "ldeorab\t" 8} } */
/* { dg-final { scan-assembler-times "ldeorlb\t" 4} } */
/* { dg-final { scan-assembler-times "ldeoralb\t" 8} } */

/* { dg-final { scan-assembler-times "ldeorh\t" 4} } */
/* { dg-final { scan-assembler-times "ldeorah\t" 8} } */
/* { dg-final { scan-assembler-times "ldeorlh\t" 4} } */
/* { dg-final { scan-assembler-times "ldeoralh\t" 8} } */

/* { dg-final { scan-assembler-times "ldeor\t" 8} */
/* { dg-final { scan-assembler-times "ldeora\t" 16} } */
/* { dg-final { scan-assembler-times "ldeorl\t" 8} } */
/* { dg-final { scan-assembler-times "ldeoral\t" 16} } */

/* { dg-final { scan-assembler-not "ldaxr\t" } } */
/* { dg-final { scan-assembler-not "stlxr\t" } } */
/* { dg-final { scan-assembler-not "dmb" } } */
