/* { dg-do compile } */

float bessel_Kn_scaled_small_x(int n)
{
  int k;
  float k_term, sum1;
  for(k=1; k<=n-1; k++)
  {
    k_term *= -1/(k * (n-k));
    sum1 += k_term;
  }
  return sum1;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
