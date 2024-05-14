// { dg-do compile { target c++11 } }

#define N 256

void f ()
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
