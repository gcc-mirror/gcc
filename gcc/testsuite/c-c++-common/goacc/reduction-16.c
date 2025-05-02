/* { dg-compile } */
#include <stdlib.h>

int foo (int n)
{
  int x[5][5];
  int y[n];
  int *z = (int *) malloc (5 * sizeof (int));

  #pragma acc parallel
  {
    #pragma acc loop reduction(+:x)
    for (int i = 0; i < 5; i++) ;
    #pragma acc loop reduction(+:y)
    for (int i = 0; i < 5; i++) ;

    #pragma acc loop reduction(+:x[2:1][0:5])
    for (int i = 0; i < 5; i++) ;
    #pragma acc loop reduction(+:x[0:5][2:1]) /* { dg-error "array section is not contiguous in 'reduction' clause" } */
    for (int i = 0; i < 5; i++) ;

    #pragma acc loop reduction(+:y[0:5])
    for (int i = 0; i < 5; i++) ;

    #pragma acc loop reduction(+:z[0:5])
    for (int i = 0; i < 5; i++) ;
  }

  return 0;
}
