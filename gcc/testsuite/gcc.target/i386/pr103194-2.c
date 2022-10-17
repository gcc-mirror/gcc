/* { dg-do compile } */
/* { dg-options "-O2" } */
#include <stdatomic.h>
#include <stdbool.h>

#define FOO(RTYPE,TYPE,MASK)						\
  __attribute__((noinline,noclone)) RTYPE				\
  atomic_fetch_or_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __atomic_fetch_or (a, mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  atomic_fetch_xor_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __atomic_fetch_xor (a, mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  atomic_xor_fetch_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __atomic_xor_fetch (a, mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  atomic_fetch_and_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __atomic_fetch_and (a, ~mask, __ATOMIC_RELAXED) & mask;	\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  sync_fetch_and_or_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __sync_fetch_and_or (a, mask) & mask;			\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  sync_fetch_and_xor_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __sync_fetch_and_xor (a, mask) & mask;			\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  sync_xor_and_fetch_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __sync_xor_and_fetch (a, mask) & mask;			\
  }									\
  __attribute__((noinline,noclone)) RTYPE				\
  sync_fetch_and_and_##TYPE##_##MASK (_Atomic TYPE* a)			\
  {									\
    TYPE mask = 1 << MASK;						\
    return __sync_fetch_and_and (a, ~mask) & mask;			\
  }									\

FOO(char, short, 0);
FOO(char, short, 7);
FOO(short, int, 0);
FOO(short, int, 15);

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*bts" 8 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btc" 16 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btr" 8 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
