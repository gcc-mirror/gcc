/* { dg-do compile { target lp64 } } */
/* { dg-options "-mavx -mcmodel=large -O3" } */

typedef double v2df __attribute__ ((vector_size (16)));
v2df f (double a, double b)
{
  v2df v;
  double *c = (double *)&v;
  *c = a;
  *(c+1) = b;
  return v;
}
void g ()
{
  v2df x = f (1.0, 1.0);
  v2df y = f (2.0, 2.0);
  for (;*(double *)&x<=8; x+=y)
    g ();
}
