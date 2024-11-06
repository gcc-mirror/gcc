/* { dg-do preprocess } */
/* { dg-error "'#include' expects" "include" { target *-*-* } .+2 } */
/* { dg-error "newline at end" "newline" { target *-*-* } .+1 } */
#include /\
