/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23" } */

[[omp::directive(error)]];			/* { dg-error "'pragma omp error' encountered" } */
[[omp::directive(error, at(compilation))]];	/* { dg-error "'pragma omp error' encountered" } */
[[omp::directive(error severity(fatal))]];	/* { dg-error "'pragma omp error' encountered" } */
[[omp::directive(error, message("my msg"))]];	/* { dg-error "'pragma omp error' encountered: my msg" } */
[[omp::directive(error severity(warning)message("another message")at(compilation))]];	/* { dg-warning "'pragma omp error' encountered: another message" } */

int
foo (int i, int x)
{
  [[omp::directive(error)]];			/* { dg-error "'pragma omp error' encountered" } */
  [[omp::directive(error, at(compilation))]];	/* { dg-error "'pragma omp error' encountered" } */
  [[omp::directive(error severity(fatal))]];	/* { dg-error "'pragma omp error' encountered" } */
  [[omp::directive(error, message("42 / 1"))]];	/* { dg-error "'pragma omp error' encountered: 42 / 1" } */
  [[omp::directive(error severity(warning) message("bar") at(compilation))]];	/* { dg-warning "'pragma omp error' encountered: bar" } */
  if (x)
    [[omp::directive(error)]];			/* { dg-error "'pragma omp error' encountered" } */
  i++;
  if (x)
    ;
  else
    [[omp::directive(error at(compilation))]];	/* { dg-error "'pragma omp error' encountered" } */
  i++;
  switch (0)
    [[omp::directive(error, severity(fatal))]];	/* { dg-error "'pragma omp error' encountered" } */
  while (0)
    [[omp::directive(error, message("42 - 1"))]];	/* { dg-error "'pragma omp error' encountered: 42 - 1" } */
  i++;
  lab:
  [[omp::directive(error, severity(warning) message("bar"), at(compilation))]];	/* { dg-warning "'pragma omp error' encountered: bar" } */
  i++;
  return i;
}
