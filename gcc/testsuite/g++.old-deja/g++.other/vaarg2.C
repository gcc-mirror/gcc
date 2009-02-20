// { dg-do run  }
// { dg-options "-Wno-abi" { target arm_eabi } }
// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Oct 1999 <nathan@acm.org>

// Make sure we can deal with POD aggregate in va_args

#include <stdarg.h>
extern "C" void abort ();

struct X {int m;};
struct Y {int a; int b; int c; int d; int e; int f;};

void fn1(va_list args)
{
  int i = va_arg (args, int);
  X x = va_arg (args, X);
  Y y = va_arg (args, Y);
  if (i != 1)
    abort ();
  if (x.m != 1)
    abort ();
  if (y.a != 1 || y.b != 2 || y.c != 3 || y.d != 4 ||
      y.e != 5 || y.f != 6)
    abort ();
}

void fn2(va_list args)
{
  const int &i = va_arg (args, int);
  const X &x = va_arg (args, X);
  const Y &y = va_arg (args, Y);
  if (i != 1)
    abort ();
  if (x.m != 1)
    abort ();
  if (y.a != 1 || y.b != 2 || y.c != 3 || y.d != 4 ||
      y.e != 5 || y.f != 6)
    abort ();
}

void
dispatch (int t, ...)
{
  va_list args;
  
  va_start (args, t);
  fn1 (args);
  va_end (args);
  
  va_start (args, t);
  fn2 (args);
  va_end (args);
}

int main ()
{
  X x = {1};
  Y y = {1, 2, 3, 4, 5, 6};
  
  dispatch (0, 1, x, y);
  
  return 0;
}
