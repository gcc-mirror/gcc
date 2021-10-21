/* PR middle-end/64888 */
/* { dg-do compile { target fopenmp } } */
/* { dg-options "-fopenmp -fsanitize=undefined" } */

int a, b;

void
foo ()
{
  int c;
#pragma omp parallel default (none) shared (a, b) private (c)
  {
    c = a / b;	/* { dg-bogus "not specified in enclosing" } */
    (void) c;
  }
#pragma omp task default (none) shared (a, b) private (c)
  {
    c = a << b;	/* { dg-bogus "not specified in enclosing" } */
    (void) c;
  }
#pragma omp teams default (none) shared (a, b)
  {
    int d[a];	/* { dg-bogus "not specified in enclosing" } */
    d[0] = 0;
    (void) d[0];
  }
}
