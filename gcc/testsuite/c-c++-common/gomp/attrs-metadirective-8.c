/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-fopenmp -std=c23" { target { c } } } */

#define N 256

void
f (void)
{
  int i;
  int a[N];

  [[omp::directive (metadirective
      when( device={kind(nohost)}: nothing )
      when( device={arch("nvptx")}: nothing)
      default( parallel for))]]
    for (i = 0; i < N; i++)
      a[i] = i;
}
