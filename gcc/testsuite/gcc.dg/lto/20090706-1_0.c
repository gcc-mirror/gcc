#include <stdarg.h>

extern void abort (void);

void foo (int size, ...)
{
  struct
    {
      struct
	{
	  char x[size];
	} e;
      unsigned r;
    } d;
  va_list ap;
  char c;
  int i;

  va_start (ap, size);
  d = va_arg (ap, typeof (d));
  c = d.e.x[3];
  if (c != '3')
    abort ();
  if (d.r != 2602)
    abort ();
  va_end (ap);
}

int main (void)
{
  int z = 5, i;
  struct { struct { char a[z]; } y; unsigned r; } x;
          
  x.y.a[0] = '0';
  x.y.a[1] = '1';
  x.y.a[2] = '2';
  x.y.a[3] = '3';
  x.y.a[4] = '4';
  x.r = 2602;
  foo (z, x);
  return 0;
}
