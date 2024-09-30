/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

void test (void *ptr)
{
  void *q;
  q = ptr;
  free (ptr);
  free (q); /* { dg-warning "double-'free' of 'q'" } */
  /* The above case requires us to handle equivalence classes in
     state transitions.  */
}
