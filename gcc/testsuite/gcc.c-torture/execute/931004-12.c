#include <stdarg.h>

struct tiny
{
  char c;
  char d;
  char e;
};

f (int n, ...)
{
  struct tiny x;
  int i;

  va_list ap;
  va_start (ap,n);
  for (i = 0; i < n; i++)
    {
      x = va_arg (ap,struct tiny);
      if (x.c != i + 10)
	abort();
      if (x.d != i + 20)
	abort();
      if (x.e != i + 30)
	abort();
    }
  {
    long x = va_arg (ap, long);
    if (x != 123)
      abort();
  }
  va_end (ap);
}

main ()
{
  struct tiny x[3];
  x[0].c = 10;
  x[1].c = 11;
  x[2].c = 12;
  x[0].d = 20;
  x[1].d = 21;
  x[2].d = 22;
  x[0].e = 30;
  x[1].e = 31;
  x[2].e = 32;
  f (3, x[0], x[1], x[2], (long) 123);
  exit(0);
}

