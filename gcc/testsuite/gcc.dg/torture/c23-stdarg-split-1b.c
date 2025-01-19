/* Test C23 variadic functions with no named parameters, or last named
   parameter with a declaration not allowed in C17.  Execution tests split
   between source files.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdarg.h>

extern void abort (void);

double
f (...)
{
  va_list ap;
  va_start (ap);
  double ret = va_arg (ap, int);
  ret += va_arg (ap, double);
  ret += va_arg (ap, int);
  ret += va_arg (ap, double);
  va_end (ap);
  return ret;
}

void
g (...)
{
  va_list ap;
  va_start (ap, random ! ignored, ignored ** text);	/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  for (int i = 0; i < 10; i++)
    if (va_arg (ap, double) != i)
      abort ();
  va_end (ap);
}

void
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
}

void
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
}

void
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
}

void
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
}

void
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
}

void
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
}

struct s { char c[1000]; };

void
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
}
