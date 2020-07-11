/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */
/* { dg-excess-errors "PR70392" { xfail c++ } } */

#include <stdbool.h>

int
main (void)
{
  int l, l2, l3, l4;
  bool b, b2, b3, b4;
  int i, i2;

  #pragma acc parallel if(l) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc parallel if(b) /* { dg-warning "is used uninitialized" "" { xfail c++ } } */
  ;

  #pragma acc kernels if(l2) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc kernels if(b2) /* { dg-warning "is used uninitialized" "" { xfail c++ } } */
  ;

  #pragma acc data if(l3) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc data if(b3) /* { dg-warning "is used uninitialized" "" { xfail c++ } } */
  ;

  #pragma acc update if(l4) self(i) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc update if(b4) self(i2) /* { dg-warning "is used uninitialized" "" { xfail c++ } } */
  ;

}
