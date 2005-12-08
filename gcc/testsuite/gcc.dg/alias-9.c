/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing -O2" } */

int a[2];

double *foo1(void)
{
  return (double *)a; /* { dg-warning "strict-aliasing" } */
}

double *foo2(void)
{
  return (double *)&a[0]; /* { dg-warning "strict-aliasing" } */
}

_Complex x;
int *bar(void)
{
  return (int *)&__imag x; /* { dg-warning "strict-aliasing" } */
}
