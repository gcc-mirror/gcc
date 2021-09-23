/* Test parsing of #pragma omp declare variant */
/* { dg-do compile } */
/* { dg-options "-std=c17 -fopenmp -pedantic-errors" } */

int f0 (int, int *, int);

int
f1 (int x)
{
  if (x)
    #pragma omp declare variant (fn0) match (user={condition(0)})
    extern int f3 (int a, int *b, int c);	/* { dg-error "must be followed by function declaration or definition" } */
  while (x < 10)
    #pragma omp declare variant (fn0) match (user={condition(0)})
    extern int f4 (int a, int *b, int c);	/* { dg-error "must be followed by function declaration or definition" } */
  {
lab:
    #pragma omp declare variant (fn0) match (user={condition(0)})
    extern int f5 (int a, int *b, int c);	/* { dg-error "must be followed by function declaration or definition" } */
    x++;
  }
  return x;
}
