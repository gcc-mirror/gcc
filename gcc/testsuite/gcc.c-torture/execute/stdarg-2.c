#include <stdarg.h>

extern void abort (void);

int foo_arg, bar_arg;
long x;
double d;
va_list gap;

void
foo (int v, va_list ap)
{
  switch (v)
    {
    case 5:
      foo_arg = va_arg (ap, int);
      foo_arg += va_arg (ap, double);
      foo_arg += va_arg (ap, long long);
      break;
    case 8:
      foo_arg = va_arg (ap, long long);
      foo_arg += va_arg (ap, double);
      break;
    case 11:
      foo_arg = va_arg (ap, int);
      foo_arg += va_arg (ap, long double);
      break;
    default:
      abort ();
    }
}

void
bar (int v)
{
  if (v == 0x4002)
    {
      if (va_arg (gap, int) != 13 || va_arg (gap, double) != -14.0)
	abort ();
    }
  bar_arg = v;
}

void
f1 (int i, ...)
{
  va_start (gap, i);
  x = va_arg (gap, long);
  va_end (gap);
}

void
f2 (int i, ...)
{
  va_start (gap, i);
  bar (i);
  va_end (gap);
}

void
f3 (int i, ...)
{
  va_list aps[10];
  va_start (aps[4], i);
  x = va_arg (aps[4], long);
  va_end (aps[4]);
}

void
f4 (int i, ...)
{
  va_list aps[10];
  va_start (aps[4], i);
  bar (i);
  va_end (aps[4]);
}

void
f5 (int i, ...)
{
  va_list aps[10];
  va_start (aps[4], i);
  foo (i, aps[4]);
  va_end (aps[4]);
}

struct A { int i; va_list g; va_list h[2]; };

void
f6 (int i, ...)
{
  struct A a;
  va_start (a.g, i);
  x = va_arg (a.g, long);
  va_end (a.g);
}

void
f7 (int i, ...)
{
  struct A a;
  va_start (a.g, i);
  bar (i);
  va_end (a.g);
}

void
f8 (int i, ...)
{
  struct A a;
  va_start (a.g, i);
  foo (i, a.g);
  va_end (a.g);
}

void
f10 (int i, ...)
{
  struct A a;
  va_start (a.h[1], i);
  x = va_arg (a.h[1], long);
  va_end (a.h[1]);
}

void
f11 (int i, ...)
{
  struct A a;
  va_start (a.h[1], i);
  bar (i);
  va_end (a.h[1]);
}

void
f12 (int i, ...)
{
  struct A a;
  va_start (a.h[1], i);
  foo (i, a.h[1]);
  va_end (a.h[1]);
}

int
main (void)
{
  f1 (1, 79);
  if (x != 79)
    abort ();
  f2 (0x4002, 13, -14.0);
  if (bar_arg != 0x4002)
    abort ();
  f3 (3, 2031L);
  if (x != 2031)
    abort ();
  f4 (4, 18);
  if (bar_arg != 4)
    abort ();
  f5 (5, 1, 19.0, 18LL);
  if (foo_arg != 38)
    abort ();
  f6 (6, 18L);
  if (x != 18L)
    abort ();
  f7 (7);
  if (bar_arg != 7)
    abort ();
  f8 (8, 2031LL, 13.0);
  if (foo_arg != 2044)
    abort ();
  f10 (9, 180L);
  if (x != 180L)
    abort ();
  f11 (10);
  if (bar_arg != 10)
    abort ();
  f12 (11, 2030, 12.0L);
  if (foo_arg != 2042)
    abort ();
  return 0;
}
