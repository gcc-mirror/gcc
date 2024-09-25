/* { dg-additional-options "-fanalyzer-call-summaries" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

void calls_free (void *p)
{
  free (p); /* { dg-warning "double-'free' of 'p'" } */
}

void test (void *q)
{
  calls_free (q);
  calls_free (q);
}
