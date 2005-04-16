// Validate that the __sync builtins are overloaded properly in templates.
// { dg-do compile }
// { dg-options "-Werror" }


#define TEST1(BUILTIN)			\
template<typename T>			\
void f##BUILTIN(T *p)			\
{					\
  __typeof(BUILTIN(p, 1)) *pp;		\
  pp = p;				\
}

TEST1(__sync_fetch_and_add)
TEST1(__sync_fetch_and_sub)
TEST1(__sync_fetch_and_or)
TEST1(__sync_fetch_and_and)
TEST1(__sync_fetch_and_xor)
TEST1(__sync_fetch_and_nand)

TEST1(__sync_add_and_fetch)
TEST1(__sync_sub_and_fetch)
TEST1(__sync_or_and_fetch)
TEST1(__sync_and_and_fetch)
TEST1(__sync_xor_and_fetch)
TEST1(__sync_nand_and_fetch)

TEST1(__sync_lock_test_and_set)

template<typename T>
void f__sync_val_compare_and_swap(T *p)
{
  __typeof(__sync_val_compare_and_swap(p, 1, 2)) *pp;
  pp = p;
}

#define TEST2(TYPE)			\
void h_##TYPE ()			\
{					\
  TYPE x;				\
  f__sync_fetch_and_add (&x);		\
  f__sync_fetch_and_sub (&x);		\
  f__sync_fetch_and_or (&x);		\
  f__sync_fetch_and_and (&x);		\
  f__sync_fetch_and_xor (&x);		\
  f__sync_fetch_and_nand (&x);		\
  f__sync_add_and_fetch (&x);		\
  f__sync_sub_and_fetch (&x);		\
  f__sync_or_and_fetch (&x);		\
  f__sync_and_and_fetch (&x);		\
  f__sync_xor_and_fetch (&x);		\
  f__sync_nand_and_fetch (&x);		\
  f__sync_lock_test_and_set (&x);	\
  f__sync_val_compare_and_swap (&x);	\
}

TEST2(int)
TEST2(long)
