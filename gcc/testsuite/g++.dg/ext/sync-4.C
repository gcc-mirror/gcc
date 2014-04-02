/* { dg-do run { target hppa*-*-hpux* *-*-linux* *-*-gnu* powerpc*-*-darwin* *-*-darwin[912]* } } */
/* { dg-require-effective-target sync_long_long_runtime } */
/* { dg-options "-fexceptions -fnon-call-exceptions -O2" } */

/* Verify that the builtin functions are correctly marked as trapping
   when using -fnon-call-exceptions.  */

#include <stdlib.h>
#include <signal.h>

typedef int ditype __attribute__ ((mode (DI)));

#define FN(IDX, RET, CALL)					\
static RET f ## IDX (void *p) __attribute__ ((noinline));	\
static RET							\
f ## IDX (void *p)						\
{								\
  return CALL;							\
}								\
static void							\
t ## IDX ()							\
{								\
  try								\
    {								\
      f ## IDX(0);						\
    }								\
  catch (...)							\
    {								\
      return;							\
    }								\
  abort();							\
}

FN(1, ditype, (__sync_fetch_and_add((ditype*)p, 1)))
FN(2, ditype, (__sync_fetch_and_sub((ditype*)p, 1)))
FN(3, ditype, (__sync_fetch_and_or((ditype*)p, 1)))
FN(4, ditype, (__sync_fetch_and_and((ditype*)p, 1)))
FN(5, ditype, (__sync_fetch_and_xor((ditype*)p, 1)))
FN(6, ditype, (__sync_fetch_and_nand((ditype*)p, 1)))

FN( 7, ditype, (__sync_add_and_fetch((ditype*)p, 1)))
FN( 8, ditype, (__sync_sub_and_fetch((ditype*)p, 1)))
FN( 9, ditype, (__sync_or_and_fetch((ditype*)p, 1)))
FN(10, ditype, (__sync_and_and_fetch((ditype*)p, 1)))
FN(11, ditype, (__sync_xor_and_fetch((ditype*)p, 1)))
FN(12, ditype, (__sync_nand_and_fetch((ditype*)p, 1)))

FN(13, bool, (__sync_bool_compare_and_swap((ditype*)p, 1, 2)))
FN(14, ditype, (__sync_val_compare_and_swap((ditype*)p, 1, 2)))

FN(15, ditype, (__sync_lock_test_and_set((ditype*)p, 1)))
FN(16, void, (__sync_lock_release((ditype*)p)))

FN(17, bool, (__atomic_test_and_set((ditype*)p, __ATOMIC_SEQ_CST)))
FN(18, void, (__atomic_clear((ditype*)p, __ATOMIC_SEQ_CST)))

FN(19, void, (__atomic_exchange((ditype*)p, (ditype*)0, (ditype*)0, __ATOMIC_SEQ_CST)))
FN(20, ditype, (__atomic_exchange_n((ditype*)p, 1, 2)))

FN(21, void, (__atomic_load((ditype*)p, (ditype*)0, __ATOMIC_SEQ_CST)))
FN(22, ditype, (__atomic_load_n((ditype*)p, __ATOMIC_SEQ_CST)))

FN(23, bool, (__atomic_compare_exchange((ditype*)p, (ditype*)0, (ditype*)0, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)))
FN(24, bool, (__atomic_compare_exchange_n((ditype*)p, (ditype*)0, 1, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)))

FN(25, void, (__atomic_store((ditype*)p, (ditype*)0, __ATOMIC_SEQ_CST)))
FN(26, void, (__atomic_store_n((ditype*)p, 1, __ATOMIC_SEQ_CST)))

FN(27, ditype, (__atomic_add_fetch((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(28, ditype, (__atomic_sub_fetch((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(29, ditype, (__atomic_and_fetch((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(30, ditype, (__atomic_nand_fetch((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(31, ditype, (__atomic_xor_fetch((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(32, ditype, (__atomic_or_fetch((ditype*)p, 1, __ATOMIC_SEQ_CST)))

FN(33, ditype, (__atomic_fetch_add((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(34, ditype, (__atomic_fetch_sub((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(35, ditype, (__atomic_fetch_and((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(36, ditype, (__atomic_fetch_nand((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(37, ditype, (__atomic_fetch_xor((ditype*)p, 1, __ATOMIC_SEQ_CST)))
FN(38, ditype, (__atomic_fetch_or((ditype*)p, 1, __ATOMIC_SEQ_CST)))

static void
handler(int)
{
  sigset_t clear;

  sigfillset (&clear);
  sigprocmask (SIG_UNBLOCK, &clear, NULL);
  throw 0;
}

int
main ()
{
  signal (SIGSEGV, handler);
  signal (SIGBUS, handler);

  t1();
  t2();
  t3();
  t4();
  t5();
  t6();
  t7();
  t8();
  t9();
  t10();
  t11();
  t12();
  t13();
  t14();
  t15();
  t16();
  t17();
  t18();
  t19();
  t20();

  exit(0);
}
