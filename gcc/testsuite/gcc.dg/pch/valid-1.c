/* { dg-options "-I. -Winvalid-pch -g" } */

#include "valid-1.h"/* { dg-warning "created with .none. debug info, but used with" } */
/* { dg-error "No such file" "no such file" { target *-*-* } 0 } */
/* { dg-error "they were invalid" "invalid files" { target *-*-* } 0 } */
/* { dg-message "terminated" "" { target *-*-* } 0 } */

int x;
