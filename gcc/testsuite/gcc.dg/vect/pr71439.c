#include "tree-vect.h"

int a, b, c;
short fn1(int p1, int p2) { return p1 + p2; }

int main() {
  check_vect ();
  a = 0;
  for (; a < 30; a = fn1(a, 4)) {
    c = b;
    b = 6;
  }

  if (c != 6)
    abort ();

  return 0;
}
