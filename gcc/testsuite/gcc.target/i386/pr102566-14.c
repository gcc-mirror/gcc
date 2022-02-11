/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */
#include <stdatomic.h>
#include <stdbool.h>
typedef long long int64;

#define FOO(TYPE,MASK)							\
  __attribute__((noinline,noclone)) TYPE				\
  atomic_fetch_or_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __atomic_fetch_or (a, mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  atomic_fetch_xor_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __atomic_fetch_xor (a, mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  atomic_xor_fetch_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __atomic_xor_fetch (a, mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  atomic_fetch_and_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __atomic_fetch_and (a, ~mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  sync_fetch_and_or_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __sync_fetch_and_or (a, mask) & mask;			\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  sync_fetch_and_xor_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __sync_fetch_and_xor (a, mask) & mask;			\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  sync_xor_and_fetch_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __sync_xor_and_fetch (a, mask) & mask;			\
  }									\
  __attribute__((noinline,noclone)) TYPE				\
  sync_fetch_and_and_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1ll << MASK;						\
    return __sync_fetch_and_and (a, ~mask) & mask;			\
  }									\


FOO(int64, 0);
FOO(int64, 32);
FOO(int64, 63);

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*bts" 6 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btc" 12 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btr" 6 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
