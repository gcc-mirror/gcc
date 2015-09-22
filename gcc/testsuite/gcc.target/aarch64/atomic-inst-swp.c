/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+lse" } */

/* Test ARMv8.1-A SWP instruction.  */

#include "atomic-inst-ops.inc"

#define TEST TEST_ONE

#define SWAP_ATOMIC(FN, TY, MODEL)					\
  TY FNNAME (FN, TY) (TY* val, TY foo)					\
  {									\
    return __atomic_exchange_n (val, foo, MODEL);			\
  }

#define SWAP_ATOMIC_NORETURN(FN, TY, MODEL)				\
  void FNNAME (FN, TY) (TY* val, TY* foo, TY* bar)			\
  {									\
    __atomic_exchange (val, foo, bar, MODEL);				\
  }


TEST (swap_atomic, SWAP_ATOMIC)
TEST (swap_atomic_noreturn, SWAP_ATOMIC_NORETURN)


/* { dg-final { scan-assembler-times "swpb\t" 4} } */
/* { dg-final { scan-assembler-times "swpab\t" 8} } */
/* { dg-final { scan-assembler-times "swplb\t" 4} } */
/* { dg-final { scan-assembler-times "swpalb\t" 8} } */

/* { dg-final { scan-assembler-times "swph\t" 4} } */
/* { dg-final { scan-assembler-times "swpah\t" 8} } */
/* { dg-final { scan-assembler-times "swplh\t" 4} } */
/* { dg-final { scan-assembler-times "swpalh\t" 8} } */

/* { dg-final { scan-assembler-times "swp\t" 8} } */
/* { dg-final { scan-assembler-times "swpa\t" 16} } */
/* { dg-final { scan-assembler-times "swpl\t" 8} } */
/* { dg-final { scan-assembler-times "swpal\t" 16} } */

/* { dg-final { scan-assembler-not "ldaxr\t" } } */
/* { dg-final { scan-assembler-not "stlxr\t" } } */
/* { dg-final { scan-assembler-not "dmb" } } */
