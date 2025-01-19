/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

#include <stdbool.h>

int
main (void)
{
  int l, l2, ls, l3, l4;
  /* { dg-note {'l' was declared here} {} { target *-*-* } .-1 } */
  /* { dg-note {'l2' was declared here} {} { target *-*-* } .-2 } */
  /* { dg-note {'ls' was declared here} {} { target *-*-* } .-3 } */
  /* { dg-note {'l3' was declared here} {} { target *-*-* } .-4 } */
  /* { dg-note {'l4' was declared here} {} { target *-*-* } .-5 } */
  bool b, b2, bs, b3, b4;
  /* { dg-note {'b' was declared here} {} { target *-*-* } .-1 } */
  /* { dg-note {'b2' was declared here} {} { target *-*-* } .-2 } */
  /* { dg-note {'bs' was declared here} {} { target *-*-* } .-3 } */
  /* { dg-note {'b3' was declared here} {} { target *-*-* } .-4 } */
  /* { dg-note {'b4' was declared here} {} { target *-*-* } .-5 } */
  int i, i2;

  #pragma acc parallel if(l) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc parallel if(b) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc kernels if(l2) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc kernels if(b2) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc serial if(ls) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc serial if(bs) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc data if(l3) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc data if(b3) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc update if(l4) self(i) /* { dg-warning "is used uninitialized" } */
  ;

  #pragma acc update if(b4) self(i2) /* { dg-warning "is used uninitialized" } */
  ;

}
