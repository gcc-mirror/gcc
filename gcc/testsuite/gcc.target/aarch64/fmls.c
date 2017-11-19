/* { dg-do compile } */
/* { dg-options "-O3" } */

#define vector __attribute__((vector_size(16)))
vector double a = {1.0,1.0};
vector double b = {2.0,2.0};
double x = 3.0;


void __attribute__ ((noinline))
vf (double x, vector double *v1, vector double *v2, vector double *result)
{
  vector double s = v1[0];
  vector double t = -v2[0];
  vector double m = {x,x};
  vector double r = t * m + s;
  result[0] = r;
}
/* { dg-final { scan-assembler-not "dup" } } */
