#include <stdarg.h>

extern void abort (void);

int foo_arg, bar_arg;
long x;
double d;
va_list gap;
struct S1 { int i; double d; int j; double e; } s1;
struct S2 { double d; long i; } s2;
int y;

void
bar (int v)
{
  bar_arg = v;
}

void
f1 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    x = va_arg (ap, long);
  va_end (ap);
}

void
f2 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    d = va_arg (ap, double);
  va_end (ap);
}

void
f3 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      x = va_arg (ap, long);
      va_end (ap);
      bar (x);
    }
}

void
f4 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      d = va_arg (ap, double);
      va_end (ap);
      bar (d + 4.0);
    }
}

void
f5 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    s1 = va_arg (ap, struct S1);
  va_end (ap);
}

void
f6 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    s2 = va_arg (ap, struct S2);
  va_end (ap);
}

void
f7 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      s1 = va_arg (ap, struct S1);
      va_end (ap);
      bar (s1.i);
    }
}

void
f8 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      s2 = va_arg (ap, struct S2);
      y = va_arg (ap, int);
      va_end (ap);
      bar (s2.i);
    }
}

int
main (void)
{
  struct S1 a1, a3;
  struct S2 a2, a4;

  f1 (7, 1L, 2L, 3L, 5L, 7L, 9L, 11L, 13L);
  if (x != 11L)
    abort ();
  f2 (6, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0);
  if (d != 32.0)
    abort ();
  f3 (2, 1L, 3L);
  if (bar_arg != 1L || x != 1L)
    abort ();
  f4 (2, 17.0, 19.0);
  if (bar_arg != 21 || d != 17.0)
    abort ();
  a1.i = 131;
  a1.j = 251;
  a1.d = 15.0;
  a1.e = 191.0;
  a3 = a1;
  a3.j = 254;
  a3.e = 178.0;
  f5 (2, a1, a3, a1);
  if (s1.i != 131 || s1.j != 254 || s1.d != 15.0 || s1.e != 178.0)
    abort ();
  f5 (3, a1, a3, a1);
  if (s1.i != 131 || s1.j != 251 || s1.d != 15.0 || s1.e != 191.0)
    abort ();
  a2.i = 138;
  a2.d = 16.0;
  a4.i = 257;
  a4.d = 176.0;
  f6 (2, a2, a4, a2);
  if (s2.i != 257 || s2.d != 176.0)
    abort ();
  f6 (3, a2, a4, a2);
  if (s2.i != 138 || s2.d != 16.0)
    abort ();
  f7 (2, a3, a1, a1);
  if (s1.i != 131 || s1.j != 254 || s1.d != 15.0 || s1.e != 178.0)
    abort ();
  if (bar_arg != 131)
    abort ();
  f8 (3, a4, a2, a2);
  if (s2.i != 257 || s2.d != 176.0)
    abort ();
  return 0;
}
