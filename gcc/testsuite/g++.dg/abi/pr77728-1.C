// { dg-do compile { target arm_eabi } }
// { dg-options "-Wpsabi" }

#include <stdarg.h>

template <int N>
struct A { double p; };

A<0> v;

template <int N>
struct B
{
  typedef A<N> T;
  int i, j;
};

struct C : public B<0> {};
struct D {};
struct E : public D, C {};
struct F : public B<1> {};
struct G : public F { static double y; };
struct H : public G {};
struct I : public D { long long z; };
struct J : public D { static double z; int i, j; };

template <int N>
struct K : public D { typedef A<N> T; int i, j; };

struct L { static double h; int i, j; };

int
fn1 (int a, B<0> b)	// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" }
{
  return a + b.i;
}

int
fn2 (int a, B<1> b)
{
  return a + b.i;
}

int
fn3 (int a, L b)	// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" }
{
  return a + b.i;
}

int
fn4 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, B<0> n, ...)
// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" "" { target *-*-* } .-1 }
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn5 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, B<1> n, ...)
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn6 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, C n, ...)
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn7 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, E n, ...)
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn8 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, H n, ...)
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn9 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, I n, ...)
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn10 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, J n, ...)
// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" "" { target *-*-* } .-1 }
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn11 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, K<0> n, ...)
// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" "" { target *-*-* } .-1 }
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

int
fn12 (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, K<2> n, ...)
{
  va_list ap;
  va_start (ap, n);
  int x = va_arg (ap, int);
  va_end (ap);
  return x;
}

void
test ()
{
  static B<0> b0;
  static B<1> b1;
  static L l;
  static C c;
  static E e;
  static H h;
  static I i;
  static J j;
  static K<0> k0;
  static K<2> k2;
  fn1 (1, b0);	// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" }
  fn2 (1, b1);
  fn3 (1, l);	// { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" }
  fn4 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, b0, 1, 2, 3, 4);
  // { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" "" { target *-*-* } .-1 }
  fn5 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, b1, 1, 2, 3, 4);
  fn6 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, c, 1, 2, 3, 4);
  fn7 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, e, 1, 2, 3, 4);
  fn8 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, h, 1, 2, 3, 4);
  fn9 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, i, 1, 2, 3, 4);
  fn10 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, j, 1, 2, 3, 4);
  // { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" "" { target *-*-* } .-1 }
  fn11 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, k0, 1, 2, 3, 4);
  // { dg-message "note: parameter passing for argument of type \[^\n\r]* changed in GCC 7\.1" "" { target *-*-* } .-1 }
  fn12 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, k2, 1, 2, 3, 4);
}
