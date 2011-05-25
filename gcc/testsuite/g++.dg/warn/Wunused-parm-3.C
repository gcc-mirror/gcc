// { dg-do compile }
// { dg-options "-Wunused -W" }
// Suppress a warning that is irrelevant to the purpose of this test.
// { dg-options "-Wunused -W -Wno-abi" { target arm_eabi } }

#include <stdarg.h>

struct A
{
  long a;
  A () : a (0) { }
  A (long x) : a (x) { }
  operator long () const { return a; }
  long operator- (const A& x) const { return a - x.a; }
};

long
fn1 (A a)
{
  return a - A (0);
}

struct B
{
  bool operator() (const int x, const int y) const throw() { return x < y; }
};

template <typename T>
bool 
fn2 (int x, int y, T z)
{
  return z (x, y);
}

bool
fn3 (void)
{
  return fn2 (1, 2, B ());
}

int
fn4 (va_list ap)
{
  return va_arg (ap, int);
}

template <typename T>
T
fn5 (va_list ap)
{
  return va_arg (ap, T);
}

int
fn6 (va_list ap)
{
  return fn5 <int> (ap);
}

template <typename T>
int
fn7 (T ap)
{
  return va_arg (ap, int);
}

int
fn8 (va_list ap)
{
  return fn7 (ap);
}
