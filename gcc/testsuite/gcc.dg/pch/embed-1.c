/* { dg-options "-std=c23 --embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */

#include "embed-1.h"

const unsigned char b[] = {
  #embed <magna-carta.txt>
};

int
main ()
{
  if (sizeof (a) != sizeof (b)
      || __builtin_memcmp (a, b, 256)
      || a[256] != 42
      || __builtin_memcmp (a + 257, b + 257, sizeof (a) - 257))
    __builtin_abort ();
}
