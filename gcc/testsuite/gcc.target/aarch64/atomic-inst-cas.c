/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+lse" } */

/* Test ARMv8.1-A CAS instruction.  */

#include "atomic-inst-ops.inc"

#define TEST TEST_TWO

#define CAS_ATOMIC(FN, TY, MODEL1, MODEL2)				\
  int FNNAME (FN, TY) (TY* val, TY* foo, TY* bar)			\
  {									\
    int model_s = MODEL1;						\
    int model_f = MODEL2;						\
    /* The success memory ordering must be at least as strong as	\
       the failure memory ordering.  */					\
    if (model_s < model_f)						\
      return 0;								\
    /* Ignore invalid memory orderings.  */				\
    if (model_f == __ATOMIC_RELEASE || model_f == __ATOMIC_ACQ_REL)	\
      return 0;								\
    return __atomic_compare_exchange_n (val, foo, bar, 0, model_s, model_f); \
  }

#define CAS_ATOMIC_NORETURN(FN, TY, MODEL1, MODEL2)			\
  void FNNAME (FN, TY) (TY* val, TY* foo, TY* bar)			\
  {									\
    int model_s = MODEL1;						\
    int model_f = MODEL2;						\
    /* The success memory ordering must be at least as strong as	\
       the failure memory ordering.  */					\
    if (model_s < model_f)						\
      return;								\
    /* Ignore invalid memory orderings.  */				\
    if (model_f == __ATOMIC_RELEASE || model_f == __ATOMIC_ACQ_REL)	\
      return;								\
    __atomic_compare_exchange_n (val, foo, bar, 0, model_s, model_f);	\
  }

TEST (cas_atomic, CAS_ATOMIC)
TEST (cas_atomic_noreturn, CAS_ATOMIC_NORETURN)


/* { dg-final { scan-assembler-times "casb\t" 4} } */
/* { dg-final { scan-assembler-times "casab\t" 20} } */
/* { dg-final { scan-assembler-times "caslb\t" 4} } */
/* { dg-final { scan-assembler-times "casalb\t" 36} } */

/* { dg-final { scan-assembler-times "cash\t" 4} } */
/* { dg-final { scan-assembler-times "casah\t" 20} } */
/* { dg-final { scan-assembler-times "caslh\t" 4} } */
/* { dg-final { scan-assembler-times "casalh\t" 36} } */

/* { dg-final { scan-assembler-times "cas\t" 8} } */
/* { dg-final { scan-assembler-times "casa\t" 40} } */
/* { dg-final { scan-assembler-times "casl\t" 8} } */
/* { dg-final { scan-assembler-times "casal\t" 72} } */

/* { dg-final { scan-assembler-not "ldaxr\t" } } */
/* { dg-final { scan-assembler-not "stlxr\t" } } */
/* { dg-final { scan-assembler-not "dmb" } } */
