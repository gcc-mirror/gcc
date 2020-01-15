/* Test path-printing in the face of macros.  */

/* { dg-additional-options "-fdiagnostics-path-format=separate-events" } */

#include "malloc-macro.h"

/* { dg-warning "double-'free' of 'ptr'" "" { target *-*-* } 2 } */
/* { dg-message "first 'free' here" "" { target *-*-* } 2 } */
/* { dg-message "second 'free' here" "" { target *-*-* } 2 } */

int test (void *ptr)
{
  WRAPPED_FREE (ptr); /* { dg-message "in expansion of macro 'WRAPPED_FREE'" } */
  WRAPPED_FREE (ptr); /* { dg-message "in expansion of macro 'WRAPPED_FREE'" } */
}
