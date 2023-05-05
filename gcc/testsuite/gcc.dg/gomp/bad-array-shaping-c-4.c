// { dg-do compile }

#include <string.h>
#include <assert.h>
#include <stdlib.h>

int main (void)
{
  float *arr = calloc (100, sizeof (float));

#pragma omp target enter data map(to: arr[:100])

  for (int j = 0; j < 10; j++)
    for (int i = 0; i < 10; i++)
      arr[j * 10 + i] = i + j * 3;

  /* No array shaping inside a statement expression.  */
#pragma omp target update to( ({ int d = 10; ([d][d]) arr; }) )
/* { dg-error {expected expression before '\[' token} "" { target *-*-* } .-1 } */
/* { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 } */

#pragma omp target exit data map(from: arr[:100])

  free (arr);

  return 0;
}
