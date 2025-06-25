#include "analyzer-decls.h"
#include <stdio.h>

int g;
int h;

int
test_pointer_to_global (FILE *f)
{
  int *p = &g;
  int *q = &h;

  fread (&g, sizeof (g), 1, f);
  fread (&h, sizeof (h), 1, f);

  return *p / *q; /* { dg-warning "-Wanalyzer-tainted-divisor" } */
}
