/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2" } */

double x;
int *foo(void)
{
  return (int *)&x; /* { dg-warning "strict-aliasing" } */
}

