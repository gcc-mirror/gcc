/* { dg-options "-I. -Winvalid-pch -g" } */

#include "valid-1.h"/* { dg-warning "created with -gnone, but used with -g" } */
/* { dg-error "No such file" "no such file" { target *-*-* } 3 } */
/* { dg-error "they were invalid" "invalid files" { target *-*-* } 3 } */

int x;
