/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-alias-details" } */
extern double cos (double);
extern double sin (double);
double f(double a)
{
  double b;
  double c,d;
  double (*fp) (double);
  if (a < 2.0)
    {
      fp = sin;
      c = fp (a);
    }
  else
    {
      c = 1.0; 
      fp = cos;
    }
  d = fp (a);
  return d + c;
}
/* The points-to set of the final function pointer should be "sin cos" */

/* { dg-final { scan-tree-dump-times "{ sin cos }" 1 "alias"} } */
