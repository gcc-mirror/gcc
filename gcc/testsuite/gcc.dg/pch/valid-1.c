/* { dg-options "-I. -Winvalid-pch -g" } */

#include "valid-1.h"/* { dg-warning "created with -gnone, but used with -g" } */
/* { dg-error "No such file" "" { target *-*-* } 3 } */
/* { dg-error "they were invalid" "" { target *-*-* } 3 } */

int x;
