/* Verify that we emit sane paths for state machine errors.  */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024); /* { dg-line malloc } */
  free (ptr); /* { dg-line first_free } */
  free (ptr); /* { dg-line second_free } */

  /* { dg-warning "double-'free' of 'ptr'"  "warning" { target *-*-* } second_free } */
  /* { dg-message "\\(1\\) allocated here" "event 1" { target *-*-* } malloc } */
  /* { dg-message "\\(2\\) first 'free' here" "event 2" { target *-*-* } first_free } */
  /* { dg-message "\\(3\\) second 'free' here; first 'free' was at \\(2\\)" "event 3" { target *-*-* } second_free } */
}
