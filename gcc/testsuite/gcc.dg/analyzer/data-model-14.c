/* FIXME: we shouldn't need this.  */
/* { dg-additional-options "-fanalyzer-fine-grained" } */

#include <stdlib.h>

void *global_ptr;

void test_1 (int i)
{
  global_ptr = malloc (1024); /* { dg-message "allocated here" } */
  *(int *)&global_ptr = i; /* { dg-warning "leak of '<unknown>'" } */
  // TODO: something better than "<unknown>" here ^^^
}

void test_2 (int i)
{
  void *p = malloc (1024); /* { dg-message "allocated here" "" { xfail *-*-* } } */
  // TODO(xfail)
  global_ptr = p;
  *(int *)&p = i;
  p = global_ptr;
  free (p);
  free (global_ptr); /* { dg-warning "double-'free' of 'p'" } */
}
