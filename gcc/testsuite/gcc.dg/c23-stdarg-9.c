/* Test C23 variadic functions with no named parameters, or last named
   parameter with a declaration not allowed in C17.  Execution tests.  */
/* { dg-do run } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

#include <stdarg.h>

struct S { int a[1024]; };

int
f1 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f2 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f3 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f4 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f5 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f6 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f7 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

int
f8 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  return r;
}

struct S
s1 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s2 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s3 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s4 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s5 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s6 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s7 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

struct S
s8 (...)
{
  int r = 0;
  va_list ap;
  va_start (ap);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  r += va_arg (ap, int);
  va_end (ap);
  struct S s = {};
  s.a[0] = r;
  return s;
}

int
b1 (void)
{
  return f8 (1, 2, 3, 4, 5, 6, 7, 8);
}

int
b2 (void)
{
  return s8 (1, 2, 3, 4, 5, 6, 7, 8).a[0];
}

int
main ()
{
  if (f1 (1) != 1 || f2 (1, 2) != 3 || f3 (1, 2, 3) != 6
      || f4 (1, 2, 3, 4) != 10 || f5 (1, 2, 3, 4, 5) != 15
      || f6 (1, 2, 3, 4, 5, 6) != 21 || f7 (1, 2, 3, 4, 5, 6, 7) != 28
      || f8 (1, 2, 3, 4, 5, 6, 7, 8) != 36)
    __builtin_abort ();
  if (s1 (1).a[0] != 1 || s2 (1, 2).a[0] != 3 || s3 (1, 2, 3).a[0] != 6
      || s4 (1, 2, 3, 4).a[0] != 10 || s5 (1, 2, 3, 4, 5).a[0] != 15
      || s6 (1, 2, 3, 4, 5, 6).a[0] != 21
      || s7 (1, 2, 3, 4, 5, 6, 7).a[0] != 28
      || s8 (1, 2, 3, 4, 5, 6, 7, 8).a[0] != 36)
    __builtin_abort ();
}
