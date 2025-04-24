// { dg-do compile }

#include <string.h>
#include <assert.h>
#include <stdlib.h>

int main (void)
{
  float *arr = calloc (100, sizeof (float));

  /* This isn't allowed.  */
#pragma omp target enter data map(to: ([10][10]) arr[:100])
/* { dg-error {expected expression before '\[' token} "" { target *-*-* } .-1 } */
/* { dg-error {'#pragma omp target enter data' must contain at least one 'map' clause} "" { target *-*-* } .-2 } */

  /* Nor this.  */
#pragma omp target exit data map(from: ([10][10]) arr[:100])
/* { dg-error {expected expression before '\[' token} "" { target *-*-* } .-1 } */
/* { dg-error {'#pragma omp target exit data' must contain at least one 'map' clause} "" { target *-*-* } .-2 } */

  free (arr);

  return 0;
}
