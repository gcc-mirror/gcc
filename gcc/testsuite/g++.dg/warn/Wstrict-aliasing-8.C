/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2 -Wall" } */

int a[2];

double *foo1(void)
{
  return (double *)a; /* { dg-warning "strict-aliasing" } */
}

double *foo2(void)
{
  return (double *)&a[0]; /* { dg-warning "strict-aliasing" } */
}

__complex__ double x;
int *bar(void)
{
  return (int *)&__imag__ x; /* { dg-warning "strict-aliasing" } */
}
