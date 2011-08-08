/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

int test_nested (int i)
{
  aligned y;

  int __attribute__ ((__noinline__, __regparm__(2))) foo (int j, int k, int l)
  {
    aligned x;

    if (check_int (&x,  __alignof__(x)) != x)
      abort ();

    if (x != 20)
      abort ();

    return i + j + k + l;
  }

  if (check_int (&y,  __alignof__(y)) != y)
    abort ();

  if (y != 20)
    abort ();

  return foo(i, i+1, i+2) * i;
}

int __attribute__ ((__noinline__, __regparm__(3), __force_align_arg_pointer__))
test_realigned (int j, int k, int l)
{
  aligned y;

  if (check_int (&y,  __alignof__(y)) != y)
    abort ();

  if (y != 20)
    abort ();

  return j + k + l;
}

int main ()
{
  if (test_nested(10) != 430)
    abort ();

  if (test_realigned(10, 11, 12) != 33)
    abort ();

  return 0;
}
