/* Test C23 variadic functions with no named parameters, or last named
   parameter with a declaration not allowed in C17.  Execution tests.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdarg.h>

extern void abort (void);
extern void exit (int);
struct s { char c[1000]; };

struct s
f (...)
{
  va_list ap;
  va_start (ap);
  double r = va_arg (ap, int);
  r += va_arg (ap, double);
  r += va_arg (ap, int);
  r += va_arg (ap, double);
  va_end (ap);
  struct s ret = {};
  ret.c[0] = r;
  ret.c[999] = 42;
  return ret;
}

struct s
g (...)
{
  va_list ap;
  va_start (ap, random ! ignored, ignored ** text);	/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  for (int i = 0; i < 10; i++)
    if (va_arg (ap, double) != i)
      abort ();
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 17;
  ret.c[999] = 58;
  return ret;
}

struct s
h1 (register int x, ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 32;
  ret.c[999] = 95;
  return ret;
}

struct s
h2 (int x(), ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 5;
  ret.c[999] = 125;
  return ret;
}

struct s
h3 (int x[10], ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 8;
  ret.c[999] = 12;
  return ret;
}

struct s
h4 (char x, ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 18;
  ret.c[999] = 28;
  return ret;
}

struct s
h5 (float x, ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 38;
  ret.c[999] = 48;
  return ret;
}

struct s
h6 (volatile long x, ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 58;
  ret.c[999] = 68;
  return ret;
}

struct s
h7 (volatile struct s x, ...)
{
  va_list ap;
  va_start (ap);
  for (int i = 0; i < 10; i++)
    {
      if (va_arg (ap, double) != i)
	abort ();
      i++;
      if (va_arg (ap, int) != i)
	abort ();
    }
  va_end (ap);
  struct s ret = {};
  ret.c[0] = 78;
  ret.c[999] = 88;
  return ret;
}

int
main ()
{
  struct s x = f (1, 2.0, 3, 4.0);
  if (x.c[0] != 10 || x.c[999] != 42)
    abort ();
  x = g (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
  if (x.c[0] != 17 || x.c[999] != 58)
    abort ();
  x = g (0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f);
  if (x.c[0] != 17 || x.c[999] != 58)
    abort ();
  x = h1 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 32 || x.c[999] != 95)
    abort ();
  x = h2 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 5 || x.c[999] != 125)
    abort ();
  x = h3 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 8 || x.c[999] != 12)
    abort ();
  x = h4 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 18 || x.c[999] != 28)
    abort ();
  x = h5 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 38 || x.c[999] != 48)
    abort ();
  x = h6 (0, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 58 || x.c[999] != 68)
    abort ();
  x = h7 ((struct s) {}, 0.0, 1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9);
  if (x.c[0] != 78 || x.c[999] != 88)
    abort ();
  exit (0);
}
