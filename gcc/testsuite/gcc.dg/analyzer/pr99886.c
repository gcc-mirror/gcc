/* Regression test for hang with -fanalyzer-verbosity=0.  */
/* { dg-additional-options "-fanalyzer-verbosity=0" } */

#include <stdlib.h>

struct coord {
  float x;
  float y;
};

void test_34 (void)
{
  float *q;
  struct coord *p = malloc (sizeof (struct coord));
  if (!p)
    return;
  p->x = 0.0f;
  q = &p->x;
  free (p);
  *q = 1.0f; /* { dg-warning "use after 'free' of 'q'" } */
};
