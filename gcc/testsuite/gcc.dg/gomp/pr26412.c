/* PR middle-end/26412 */
/* { dg-do compile } */

extern double a[];
extern int b;

double
test (void)
{
  int i;
  double c = 0;

#pragma omp parallel for private(i) reduction(+:c)
  for (i = 0; i < 10000; i++)
    c += a[b];

  return c;
}
