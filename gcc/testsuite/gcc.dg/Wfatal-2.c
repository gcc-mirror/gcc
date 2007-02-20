/* { dg-do compile } */
/* { dg-options "-Woverflow -Wdiv-by-zero -Werror -Wfatal-errors" } */
#include <limits.h>

int i = INT_MAX + 1; /* { dg-error "integer overflow in expression" } */
int k = 1 / 0; 
int j = INT_MIN - 1;
/* { dg-warning "being treated as errors" "" { target *-*-* } 0 } */
/* { dg-warning "terminated due to -Wfatal-errors" "" { target *-*-* } 0 } */
