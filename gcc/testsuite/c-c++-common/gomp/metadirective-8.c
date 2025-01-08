/* { dg-do compile } */

#define N 256

void
f (void)
{
  int i;
  int a[N];

  #pragma omp metadirective \
      when( device={kind(nohost)}: nothing ) \
      when( device={arch("nvptx")}: nothing) \
      default( parallel for)
    for (i = 0; i < N; i++)
      a[i] = i;
}
