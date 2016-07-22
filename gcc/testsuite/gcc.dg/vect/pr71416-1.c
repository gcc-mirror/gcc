#include "tree-vect.h"

short a;
char b, e;
static short c;
int d, f;
int main() {
  short g;
  check_vect ();
  for (; e; ++e) {
    d = a;
    f = 0;
    if (b)
      d = f = g >= c;
  }
  return 0;
}

