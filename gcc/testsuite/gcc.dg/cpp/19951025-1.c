/* { dg-do preprocess } */
/* { dg-error "include expects" "" { target *-*-* } 5 } */
/* { dg-error "newline at end" "" { target *-*-* } 5 } */
#include /\
