/* Verify that we emit sane paths for state machine errors.  */

#include <stdlib.h>

void test_2 (void *ptr)
{
  free (ptr); /* { dg-line first_free } */
  free (ptr); /* { dg-line second_free } */

  /* { dg-warning "double-'free' of 'ptr'"  "warning" { target *-*-* } second_free } */
  /* { dg-message "\\(1\\) first 'free' here" "event 1" { target *-*-* } first_free } */
  /* { dg-message "\\(2\\) second 'free' here; first 'free' was at \\(1\\)" "event 2" { target *-*-* } second_free } */
}
