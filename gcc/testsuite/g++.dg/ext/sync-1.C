// Validate that the __sync builtins are overloaded properly.
// { dg-do compile }
// { dg-options "-Werror" }

#define TEST1(TYPE, BUILTIN)		\
void t_##TYPE##BUILTIN(TYPE *p)		\
{					\
  __typeof(BUILTIN(p, 1)) *pp;		\
  pp = p;				\
}

#define TEST2(BUILTIN)		\
  TEST1(int, BUILTIN)		\
  TEST1(long, BUILTIN)

TEST2(__sync_fetch_and_add)
TEST2(__sync_fetch_and_sub)
TEST2(__sync_fetch_and_or)
TEST2(__sync_fetch_and_and)
TEST2(__sync_fetch_and_xor)
TEST2(__sync_fetch_and_nand)

TEST2(__sync_add_and_fetch)
TEST2(__sync_sub_and_fetch)
TEST2(__sync_or_and_fetch)
TEST2(__sync_and_and_fetch)
TEST2(__sync_xor_and_fetch)
TEST2(__sync_nand_and_fetch)

TEST2(__sync_lock_test_and_set)

#define TEST3(TYPE)					\
void t_##TYPE##__sync_val_compare_and_swap(TYPE *p)	\
{							\
  __typeof(__sync_val_compare_and_swap(p, 1, 2)) *pp;	\
  pp = p;						\
}

TEST3(int)
TEST3(long)
