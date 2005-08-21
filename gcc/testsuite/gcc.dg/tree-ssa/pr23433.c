/* { dg-do compile } */
/* { dg-options "-O2" } */

double transport_sumexp(int numexp)
{
  int k,j;
  double xk1 = 1.0;
  for(k=1; k<=numexp;k++)
    for(j=1;j<=3;j++)
      xk1 += 1.0;
  return xk1;
}
