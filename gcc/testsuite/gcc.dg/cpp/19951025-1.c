/* { dg-do preprocess } */
/* { dg-error "include expects" "include" { target *-*-* } 4 } */
/* { dg-error "newline at end" "newline" { target *-*-* } 4 } */
#include /\
