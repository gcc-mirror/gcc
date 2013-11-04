/* { dg-do run { target hppa*-*-hpux* *-*-linux* *-*-gnu* powerpc*-*-darwin* *-*-darwin[912]* } } */
/* { dg-options "-fexceptions -fnon-call-exceptions -O2" } */

/* Verify that the builtin functions are correctly marked as trapping
   when using -fnon-call-exceptions.  */

#include <stdlib.h>
#include <signal.h>

typedef int int32_t __attribute__ ((mode (SI)));
typedef int int64_t __attribute__ ((mode (DI)));

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

FN(1, int64_t, (__sync_fetch_and_add((int64_t*)p, 1)))
FN(2, int64_t, (__sync_fetch_and_sub((int64_t*)p, 1)))
FN(3, int64_t, (__sync_fetch_and_or((int64_t*)p, 1)))
FN(4, int64_t, (__sync_fetch_and_and((int64_t*)p, 1)))
FN(5, int64_t, (__sync_fetch_and_xor((int64_t*)p, 1)))
FN(6, int64_t, (__sync_fetch_and_nand((int64_t*)p, 1)))

FN( 7, int64_t, (__sync_add_and_fetch((int64_t*)p, 1)))
FN( 8, int64_t, (__sync_sub_and_fetch((int64_t*)p, 1)))
FN( 9, int64_t, (__sync_or_and_fetch((int64_t*)p, 1)))
FN(10, int64_t, (__sync_and_and_fetch((int64_t*)p, 1)))
FN(11, int64_t, (__sync_xor_and_fetch((int64_t*)p, 1)))
FN(12, int64_t, (__sync_nand_and_fetch((int64_t*)p, 1)))

FN(13, bool, (__sync_bool_compare_and_swap((int64_t*)p, 1, 2)))
FN(14, int64_t, (__sync_val_compare_and_swap((int64_t*)p, 1, 2)))

FN(15, int64_t, (__sync_lock_test_and_set((int64_t*)p, 1)))
FN(16, void, (__sync_lock_release((int64_t*)p)))

FN(17, bool, (__atomic_test_and_set((int64_t*)p, __ATOMIC_SEQ_CST)))
FN(18, void, (__atomic_clear((int64_t*)p, __ATOMIC_SEQ_CST)))

FN(19, void, (__atomic_exchange((int64_t*)p, (int64_t*)0, (int64_t*)0, __ATOMIC_SEQ_CST)))
FN(20, int64_t, (__atomic_exchange_n((int64_t*)p, 1, 2)))

FN(21, void, (__atomic_load((int64_t*)p, (int64_t*)0, __ATOMIC_SEQ_CST)))
FN(22, int64_t, (__atomic_load_n((int64_t*)p, __ATOMIC_SEQ_CST)))

FN(23, bool, (__atomic_compare_exchange((int64_t*)p, (int64_t*)0, (int64_t*)0, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)))
FN(24, bool, (__atomic_compare_exchange_n((int64_t*)p, (int64_t*)0, 1, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)))

FN(25, void, (__atomic_store((int64_t*)p, (int64_t*)0, __ATOMIC_SEQ_CST)))
FN(26, void, (__atomic_store_n((int64_t*)p, 1, __ATOMIC_SEQ_CST)))

FN(27, int64_t, (__atomic_add_fetch((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(28, int64_t, (__atomic_sub_fetch((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(29, int64_t, (__atomic_and_fetch((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(30, int64_t, (__atomic_nand_fetch((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(31, int64_t, (__atomic_xor_fetch((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(32, int64_t, (__atomic_or_fetch((int64_t*)p, 1, __ATOMIC_SEQ_CST)))

FN(33, int64_t, (__atomic_fetch_add((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(34, int64_t, (__atomic_fetch_sub((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(35, int64_t, (__atomic_fetch_and((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(36, int64_t, (__atomic_fetch_nand((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(37, int64_t, (__atomic_fetch_xor((int64_t*)p, 1, __ATOMIC_SEQ_CST)))
FN(38, int64_t, (__atomic_fetch_or((int64_t*)p, 1, __ATOMIC_SEQ_CST)))

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
