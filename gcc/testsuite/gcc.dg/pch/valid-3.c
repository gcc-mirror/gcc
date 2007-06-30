/* { dg-options "-I. -Winvalid-pch -fno-unit-at-a-time" } */

#include "valid-3.h"/* { dg-warning "settings for -funit-at-a-time do not match" } */
/* { dg-error "No such file" "" { target *-*-* } 3 } */
/* { dg-error "they were invalid" "" { target *-*-* } 3 } */
int x;
