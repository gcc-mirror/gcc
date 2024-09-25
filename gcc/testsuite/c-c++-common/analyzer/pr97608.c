/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void *f (void)
{
  void *p = malloc (8);
  if (p == NULL)
    abort ();
  return (void *) ((char *) p + 0);
}

void *g (void)
{
  void *p = malloc (8);
  if (p == NULL)
    abort ();
  return (void *) ((char *) p + 1);
}
