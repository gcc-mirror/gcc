#include <stdarg.h>

extern void abort (void);

int foo_arg, bar_arg;
long x;
double d;
va_list gap;
va_list *pap;

void
foo (int v, va_list ap)
{
  switch (v)
    {
    case 5: foo_arg = va_arg (ap, int); break;
    default: abort ();
    }
}

void
bar (int v)
{
  if (v == 0x4006)
    {
      if (va_arg (gap, double) != 17.0
	  || va_arg (gap, long) != 129L)
	abort ();
    }
  else if (v == 0x4008)
    {
      if (va_arg (*pap, long long) != 14LL
	  || va_arg (*pap, long double) != 131.0L
	  || va_arg (*pap, int) != 17)
	abort ();
    }
  bar_arg = v;
}

void
f0 (int i, ...)
{
}

void
f1 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  va_end (ap);
}

void
f2 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  x = va_arg (ap, long);
  bar (x);
  va_end (ap);
}

void
f3 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  d = va_arg (ap, double);
  va_end (ap);
}

void
f4 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  x = va_arg (ap, double);
  foo (i, ap);
  va_end (ap);
}

void
f5 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  va_copy (gap, ap);
  bar (i);
  va_end (ap);
  va_end (gap);
}

void
f6 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  va_arg (ap, long);
  va_arg (ap, long);
  x = va_arg (ap, long);
  bar (x);
  va_end (ap);
}

void
f7 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  pap = &ap;
  bar (i);
  va_end (ap);
}

void
f8 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  pap = &ap;
  bar (i);
  d = va_arg (ap, double);
  va_end (ap);
}

int
main (void)
{
  f0 (1);
  f1 (2);
  d = 31.0;
  f2 (3, 28L);
  if (bar_arg != 28 || x != 28)
    abort ();
  f3 (4, 131.0);
  if (d != 131.0)
    abort ();
  f4 (5, 16.0, 128);
  if (x != 16 || foo_arg != 128)
    abort ();
  f5 (0x4006, 17.0, 129L);
  if (bar_arg != 0x4006)
    abort ();
  f6 (7, 12L, 14L, -31L);
  if (bar_arg != -31)
    abort ();
  f7 (0x4008, 14LL, 131.0L, 17, 26.0);
  if (bar_arg != 0x4008)
    abort ();
  f8 (0x4008, 14LL, 131.0L, 17, 27.0);
  if (bar_arg != 0x4008 || d != 27.0)
    abort ();
  return 0;
}
