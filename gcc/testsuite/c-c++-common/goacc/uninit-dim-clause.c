/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

#include <stdbool.h>

int
main (void)
{
  int i, j, k;

  #pragma acc parallel num_gangs(i) /* { dg-warning "is used uninitialized in this function" } */
  ;

  #pragma acc parallel num_workers(j) /* { dg-warning "is used uninitialized in this function" } */
  ;

  #pragma acc parallel vector_length(k) /* { dg-warning "is used uninitialized in this function" } */
  ;
}
