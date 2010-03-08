/* { dg-options "-O1 -floop-parallelize-all" } */

double lagrange(const double x[],
                const double y[],
                long n,
                double xval)
{
  long i, j;
  double yval = 0.;

  for( i=0; i < n; i++ )
    {
      double l = 1.;
      for( j=0; j < n; j++ )
	if( i != j )
	  l *= (xval-x[j])/(x[i]-x[j]);
      yval += y[i]*l;
    }
  return yval;
}
