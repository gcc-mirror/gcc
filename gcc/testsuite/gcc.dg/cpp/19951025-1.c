/* { dg-do preprocess } */
/* { dg-error "include expects" "" { target *-*-* } 4 } */
/* { dg-warning "no newline" "" { target *-*-* } 5 } */
#include /\
