// { dg-do run  }
#include <stdarg.h>

extern "C" void abort();

template <class T>
T* f(T t, ...)
{
  va_list ap;

  va_start(ap, t);
  T* r = va_arg(ap, T*);
  va_end(ap);

  return r;
}


struct S 
{
};

int main()
{
  S s;

  if (f(s, &s) != &s)
    abort();
}
