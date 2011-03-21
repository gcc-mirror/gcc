// PR c++/47022
// { dg-do compile }

#include <cstdarg>

template <typename T>
void
f1 (T *p, va_list ap)
{
  *p = va_arg (ap, long double);
  *p += va_arg (ap, double);
}

template <typename T>
void
f2 (T *p, va_list ap)
{
  *p = __real__ va_arg (ap, _Complex int);
  *p += __imag__ va_arg (ap, _Complex double);
  *p += __imag__ va_arg (ap, _Complex long double);
}

template <typename T>
void
f3 (T *p, va_list ap)
{
  *p = va_arg (ap, T);
}

void
foo (int x, va_list ap)
{
  if (x == 0)
    {
      long double ld;
      f1 (&ld, ap);
    }
  else if (x == 1)
    {
      int i;
      f2 (&i, ap);
    }
  else if (x == 2)
    {
      long double ld;
      f3 (&ld, ap);
    }
  else if (x == 3)
    {
      _Complex double cd;
      f3 (&cd, ap);
    }
}
