/* Test we're able use __atomic_fetch_* where possible and verify
   we generate correct code.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors -fdump-tree-original" } */
/* { dg-xfail-run-if "PR97444: stack atomics" { nvptx*-*-* } }*/

#include <stdatomic.h>
#include <limits.h>

extern void abort (void);

#define TEST_TYPE(TYPE, MAX)				\
  do							\
    {							\
      struct S { char a[(MAX) + 1]; };			\
      TYPE t = 1;					\
      struct S a[2][2];					\
      struct S (*_Atomic p)[2] = &a[0];			\
      p += t;						\
      if (p != &a[1])					\
	abort ();					\
      p -= t;						\
      if (p != &a[0])					\
	abort ();					\
    }							\
  while (0)

int
main (void)
{
  TEST_TYPE (signed char, UCHAR_MAX);
  TEST_TYPE (signed short, USHRT_MAX);
}

/* { dg-final { scan-tree-dump-not "__atomic_compare_exchange" "original" } } */
