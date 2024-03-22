/* Verify that we emit sane paths for state machine errors.  */

#include <stdlib.h>

int *test_4 (void)
{
  int *ptr = (int *)malloc (sizeof (int)); /* { dg-line malloc } */
  if (ptr) /* { dg-line cond } */
    *ptr = 42;
  else
    *ptr = 43; /* { dg-line on_null_ptr } */
  return ptr;

  /* { dg-warning "dereference of NULL 'ptr'" "warning" { target *-*-* } on_null_ptr } */
  /* { dg-message "\\(1\\) allocated here" "event 1" { target *-*-* } malloc } */
  /* { dg-message "\\(2\\) assuming 'ptr' is NULL" "event 2" { target *-*-* } cond } */
  /* { dg-message "\\(3\\) following 'false' branch \\(when 'ptr' is NULL\\)\\.\\.\\." "event 3" { target *-*-* } cond } */
  /* { dg-message "\\(4\\) \\.\\.\\.to here" "event 4" { target *-*-* } on_null_ptr } */
  /* { dg-message "\\(5\\) dereference of NULL 'ptr'" "event 5" { target *-*-* } on_null_ptr } */
}
