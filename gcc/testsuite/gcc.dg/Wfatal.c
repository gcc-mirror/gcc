/* { dg-do compile } */
/* { dg-options "-Woverflow -Werror=div-by-zero -Wfatal-errors" } */
#include <limits.h>

int i = INT_MAX + 1; /* { dg-warning "integer overflow in expression" } */
int k = 1 / 0; /* { dg-error "division by zero" } */
int j = INT_MIN - 1;
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
/* { dg-message "terminated due to -Wfatal-errors" "" { target *-*-* } 0 } */



