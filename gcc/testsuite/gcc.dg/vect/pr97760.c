#include "tree-vect.h"

int b=1;
static int *g = &b;

void __attribute__((noipa))
h (unsigned int n)
{
  int i = 3;
  int f = 3;
  for (; f <= 50; f += 4) {
    i += 4;
    *g = i;
    i += n;
  }
}

int main ()
{
  check_vect ();

  h (9);
  if (*g != 150 || b != 150)
    __builtin_abort ();
  return 0;
}
