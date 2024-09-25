/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

extern void foo (void *);

void
test_1 (int nr_passes)
{
  int pass;
  void *p;

  for (pass = 0; pass < nr_passes; ++pass) {
    p = malloc (1024);
    foo (p);
    free (p);
  }
}
