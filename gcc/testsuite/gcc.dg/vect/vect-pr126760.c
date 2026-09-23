#include "tree-vect.h"

long __attribute__((noipa))
foo (short a)
{
  long c = a;
  unsigned f;
  int d = 0, e;
  for (; d < 6; d++) {
    e = 0;
    while (e < 6) {
      e++;
      f = c;
      switch (f) {
      case 1:
        c = c + d;
      case 3:
        break;
      default:
        c = c + d;
      }
    }
  }
  return c;
}

int main()
{
  check_vect ();
  if (foo (0) != 3)
    __builtin_abort ();
  return 0;
}
