#include "tree-vect.h"

signed short array[0x10001];

__attribute__((noipa)) void
foo(signed short *a)
{
  signed short b = 64;
  for (unsigned i = 0; i < 0x10001; ++i) {
    a[i] = b;
    b >>= 1;
  }
}

int main(void)
{
  check_vect ();
  foo(array);
  if (array[0x10000] != 0)
    __builtin_abort ();
  return 0;
}
