/* { dg-do preprocess } */
/* { dg-error "include expects" "" { target *-*-* } 4 } */
/* { dg-error "newline at end" "" { target *-*-* } 4 } */
#include /\
