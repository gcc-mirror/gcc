/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

void recursive_free (void *ptr)
{
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
  recursive_free (ptr); /* { dg-warning "infinite recursion" } */
}
