/* { dg-do compile } */

int cond;

int main (void)
{
  int *arr;

  /* No array shaping inside conditional operator.  */
#pragma omp target update to(cond ? ([3][9]) arr : ([2][7]) arr)
/* { dg-error {expected expression before '\[' token} "" { target *-*-* } .-1 } */
/* { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-2 } */

  return 0;
}
