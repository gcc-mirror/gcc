/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-ftree-vectorize -fno-vect-cost-model" } */
/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

double getdot(int n, const double *x, int inc_x, const double *y)
{
  int i, ix = 0;
  double dot[4] = { 0.0, 0.0, 0.0, 0.0 } ;

  for(i = 0; i < n; i++) {
      dot[0] += x[ix]   * y[ix]   ;
      dot[1] += x[ix+1] * y[ix+1] ;
      dot[2] += x[ix]   * y[ix+1] ;
      dot[3] += x[ix+1] * y[ix]   ;
      ix += inc_x ;
  }

  return dot[0] + dot[1] + dot[2] + dot[3];
}

int main()
{
  double x[2] = {0, 0}, y[2] = {0, 0};
  if (getdot(1, x, 4096*4096, y) != 0.)
    __builtin_abort ();
  return 0;
}
