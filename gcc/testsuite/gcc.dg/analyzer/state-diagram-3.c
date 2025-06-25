#include "analyzer-decls.h"

int
inner (int *p)
{
  return *p; /* { dg-warning "-Wanalyzer-use-of-uninitialized-value" } */
}

int
middle (int *q)
{
  return inner (q);
}

int
outer ()
{
  int i;
  return middle (&i);
}
