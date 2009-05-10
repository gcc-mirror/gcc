/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */
double c;
double d;
double *f(int a)
{
  if(a) return &c;
  return &d;
}
