/* { dg-options "-I. -Winvalid-pch -fexceptions" } */
/* { dg-require-effective-target exceptions } */
#include "valid-2.h" /* { dg-warning "settings for -fexceptions do not match" } */
/* { dg-error "No such file" "no such file" { target *-*-* } 0 } */
/* { dg-error "they were invalid" "invalid files" { target *-*-* } 0 } */
/* { dg-message "terminated" "" { target *-*-* } 0 } */
int x;
