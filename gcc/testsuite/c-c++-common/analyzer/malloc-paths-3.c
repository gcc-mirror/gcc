/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

/* Verify that we emit sane paths for state machine errors.  */

#include <stdlib.h>

int *test_3 (void)
{
  int *ptr = (int *)malloc (sizeof (int)); /* { dg-line malloc } */
  *ptr = 42; /* { dg-line unchecked_deref } */
  return ptr;

  /* { dg-warning "dereference of possibly-NULL 'ptr'" "warning" { target *-*-* } unchecked_deref } */
  /* { dg-message "\\(1\\) this call could return NULL" "event 1" { target *-*-* } malloc } */
  /* { dg-message "\\(2\\) 'ptr' could be NULL" "event 2" { target *-*-* } unchecked_deref } */
}
