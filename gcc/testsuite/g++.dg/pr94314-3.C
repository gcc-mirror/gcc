/* PR c++/94314.  */
/* { dg-do run } */
/* { dg-options "-O2 --param early-inlining-insns=100 -fdump-tree-cddce-details" } */
/* { dg-additional-options "-fdelete-null-pointer-checks" } */

#include <stdio.h>

volatile int idx;

struct base
{
  __attribute__ ((malloc, noinline)) static void *
  operator new (__SIZE_TYPE__ sz)
  {
    return ::operator new (sz);
  }

  __attribute__ ((noinline)) static void operator delete (void *ptr)
  {
    int c = count[idx];
    count[idx] = c - 1;
    ::operator delete (ptr);
  }
  volatile static int count[2];
};

volatile int base::count[2] = {0, 0};

struct B : base
{
  static void *operator new (__SIZE_TYPE__ sz)
  {
    int c = count[idx];
    count[idx] = c + 1;
    return base::operator new (sz);
  }
};

volatile int c = 1;

int
main ()
{
  for (int i; i < c; i++)
    {
      idx = 0;
      delete new B;
      if (B::count[0] != 0)
	__builtin_abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-not "Deleting : operator delete" "cddce1"} } */
