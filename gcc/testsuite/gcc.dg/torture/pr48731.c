/* { dg-do compile } */

#include <stdarg.h>

int blah(int a, ...)
{
  va_list va;
  va_start(va,a);
  if (a == 0)
    return -1;
  else 
    {
      int i;
      for (i = 0; i < a; i++)
	va_arg(va,int);
      return va_arg(va,int);
    }
}

__attribute((flatten))
int blah2(int b, int c)
{
  return blah(2, b, c);
}
