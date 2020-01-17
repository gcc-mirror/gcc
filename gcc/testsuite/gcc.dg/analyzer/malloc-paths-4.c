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

  /* { dg-warning "dereference of NULL 'ptr'" "" { target *-*-* } on_null_ptr } */
  /* { dg-message "\\(1\\) allocated here" "" { target *-*-* } malloc } */
  /* { dg-message "\\(2\\) assuming 'ptr' is NULL" "" { target *-*-* } cond } */
  /* { dg-message "\\(3\\) following 'false' branch \\(when 'ptr' is NULL\\)\\.\\.\\." "" { target *-*-* } cond } */
  /* { dg-message "\\(4\\) \\.\\.\\.to here" "" { target *-*-* } on_null_ptr } */
  /* { dg-message "\\(5\\) dereference of NULL 'ptr'" "" { target *-*-* } on_null_ptr } */
}
