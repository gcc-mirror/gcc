/* { dg-do compile } */

#define N       50
#define CHUNKSIZE   5

main ()
{
  int i, chunk;
  float c[N];

  chunk = CHUNKSIZE;
#pragma omp parallel for shared (c, chunk) schedule (dynamic, chunk)
  for (i = 0; i < N; i++)
    c[i] = i;

  return 0;
}
