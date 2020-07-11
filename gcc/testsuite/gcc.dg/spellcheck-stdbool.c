/* { dg-options "-std=c99" } */
/* Missing <stdbool.h>.  */

bool b; /* { dg-error "unknown type name 'bool'" } */
/* { dg-message "'bool' is defined in header '<stdbool.h>'; did you forget to '#include <stdbool.h>'?" "" { target *-*-* } .-1 } */

int test_true (void)
{
  return true; /* { dg-error "'true' undeclared" } */
  /* { dg-message "'true' is defined in header '<stdbool.h>'; did you forget to '#include <stdbool.h>'?" "" { target *-*-* } .-1 } */
}

int test_false (void)
{
  return false; /* { dg-error "'false' undeclared" } */
  /* { dg-message "'false' is defined in header '<stdbool.h>'; did you forget to '#include <stdbool.h>'?" "" { target *-*-* } .-1 } */
}
