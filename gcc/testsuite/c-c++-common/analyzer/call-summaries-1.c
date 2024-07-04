/* { dg-additional-options "-fanalyzer-call-summaries" } */

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
