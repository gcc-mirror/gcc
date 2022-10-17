#pragma omp error			/* { dg-error "'pragma omp error' encountered" } */
#pragma omp error at(compilation)	/* { dg-error "'pragma omp error' encountered" } */
#pragma omp error severity(fatal)	/* { dg-error "'pragma omp error' encountered" } */
#pragma omp error message("my msg")	/* { dg-error "'pragma omp error' encountered: my msg" } */
#pragma omp error severity(warning)message("another message")at(compilation)	/* { dg-warning "'pragma omp error' encountered: another message" } */

struct S {
  #pragma omp error			/* { dg-error "'pragma omp error' encountered" } */
  #pragma omp error at(compilation)	/* { dg-error "'pragma omp error' encountered" } */
  #pragma omp error severity(fatal)	/* { dg-error "'pragma omp error' encountered" } */
  #pragma omp error message("42")	/* { dg-error "'pragma omp error' encountered: 42" } */
  #pragma omp error severity(warning), message("foo"), at(compilation)	/* { dg-warning "'pragma omp error' encountered: foo" } */
  int s;
};

int
foo (int i, int x)
{
  #pragma omp error			/* { dg-error "'pragma omp error' encountered" } */
  #pragma omp error at(compilation)	/* { dg-error "'pragma omp error' encountered" } */
  #pragma omp error severity(fatal)	/* { dg-error "'pragma omp error' encountered" } */
  #pragma omp error message("42 / 1")	/* { dg-error "'pragma omp error' encountered: 42 / 1" } */
  #pragma omp error severity(warning) message("bar") at(compilation)	/* { dg-warning "'pragma omp error' encountered: bar" } */
  if (x)
    #pragma omp error			/* { dg-error "'pragma omp error' encountered" } */
    i++;
  if (x)
    ;
  else
    #pragma omp error at(compilation)	/* { dg-error "'pragma omp error' encountered" } */
    i++;
  switch (0)
    #pragma omp error severity(fatal)	/* { dg-error "'pragma omp error' encountered" } */
    {
    default:
      break;
    }
  while (0)
    #pragma omp error message("42 - 1")	/* { dg-error "'pragma omp error' encountered: 42 - 1" } */
    i++;
  lab:
  #pragma omp error severity(warning) message("bar") at(compilation)	/* { dg-warning "'pragma omp error' encountered: bar" } */
    i++;
  return i;
}
