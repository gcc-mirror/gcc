/* PR tree-optimization/69820 */

#include "tree-vect.h"

unsigned int a[100];
long long int b[100];
unsigned short c[100];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  for (i = 0; i < 100; ++i)
    b[i] = a[i] * (c[i] * (_Bool) c[i]);
}

int
main ()
{
  int i;
  if (__SIZEOF_INT__ * __CHAR_BIT__ != 32)
    return 0;
  check_vect ();
  for (i = 0; i < 100; ++i)
    {
      a[i] = 3489456818U;
      b[i] = 0x1eadbeefbeefdeadLL;
      c[i] = 38364;
    }
  foo ();
#pragma GCC novector
  for (i = 0; i < 100; ++i)
    if (b[i] != 0xed446af8U)
      __builtin_abort ();
  return 0;
}
