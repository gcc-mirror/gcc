/* { dg-additional-options "-fdiagnostics-path-format=none -fanalyzer-verbosity=1" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

void
two_frees (void *p, void *q)
{
  free (p);
  free (q); /* { dg-warning "double-'free' of 'q'" } */
  /* TODO: could be useful to identify that p == q when called from 'test'.  */
}

extern void do_stuff (void);

void test (void *ptr)
{
  two_frees (ptr, ptr);
}
