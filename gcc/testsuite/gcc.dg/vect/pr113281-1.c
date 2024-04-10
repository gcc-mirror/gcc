#include "tree-vect.h"

unsigned char a;

int main() {
  check_vect ();

  short b = a = 0;
  for (; a != 19; a++)
    if (a)
      b = 32872 >> a;

  if (b == 0)
    return 0;
  else
    return 1;
}
