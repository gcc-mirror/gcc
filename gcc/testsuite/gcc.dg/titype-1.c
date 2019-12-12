/* { dg-do run } */

/* Not all platforms support TImode integers.  */
#if (defined(__LP64__) && !defined(__hppa__)) || defined(_WIN64)
typedef int TItype __attribute__ ((mode (TI)));
#else
typedef long TItype;
#endif

#include <stdarg.h>

extern void abort(void);


void foo(int i, ...)
{
  TItype q;
  va_list va;

  va_start(va, i);
  q = va_arg(va, TItype);
  va_end(va);

  if (q != 5)
    abort();
}

int main(void)
{
  TItype q = 5;

  foo(1, q);
  return 0;
}
