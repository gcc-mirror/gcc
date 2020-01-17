/* Verify that we emit sane paths for state machine errors.  */

#include <stdlib.h>

int *test_3 (void)
{
  int *ptr = (int *)malloc (sizeof (int)); /* { dg-line malloc } */
  *ptr = 42; /* { dg-line unchecked_deref } */
  return ptr;

  /* { dg-warning "dereference of possibly-NULL 'ptr'" "" { target *-*-* } unchecked_deref } */
  /* { dg-message "\\(1\\) this call could return NULL" "" { target *-*-* } malloc } */
  /* { dg-message "\\(2\\) 'ptr' could be NULL" "" { target *-*-* } unchecked_deref } */
}
