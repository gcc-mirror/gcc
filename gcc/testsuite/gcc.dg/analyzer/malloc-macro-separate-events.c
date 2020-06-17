/* Test path-printing in the face of macros.  */

/* { dg-additional-options "-fdiagnostics-path-format=separate-events" } */

#include "malloc-macro.h"

/* { dg-warning "double-'free' of 'ptr'" "warning" { target *-*-* } 2 } */
/* { dg-message "first 'free' here" "1st free event" { target *-*-* } 2 } */
/* { dg-message "second 'free' here" "2nd free event" { target *-*-* } 2 } */

int test (void *ptr)
{
  WRAPPED_FREE (ptr); /* { dg-message "in expansion of macro 'WRAPPED_FREE'" } */
  WRAPPED_FREE (ptr); /* { dg-message "in expansion of macro 'WRAPPED_FREE'" } */
}
