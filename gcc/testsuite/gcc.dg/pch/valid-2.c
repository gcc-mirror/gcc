/* { dg-options "-I. -Winvalid-pch -fexceptions" } */

#include "valid-2.h" /* { dg-warning "settings for -fexceptions do not match" } */
/* { dg-error "No such file" "" { target *-*-* } 3 } */
/* { dg-error "they were invalid" "" { target *-*-* } 3 } */
int x;
