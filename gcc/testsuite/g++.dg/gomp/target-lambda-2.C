// We use 'auto' without a function return type, so specify dialect here
// { dg-additional-options "-std=c++14 -fdump-tree-gimple" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include <cstdlib>

#define N 10
int main (void)
{
  int X, Y;
  #pragma omp target map(from: X, Y)
  {
    int x = 0, y = 0;

    for (int i = 0; i < N; i++)
      [&] (int v) { x += v; } (i);

    auto yinc = [&y] { y++; };
    for (int i = 0; i < N; i++)
      yinc ();

    X = x;
    Y = y;
  }

  int Xs = 0;
  for (int i = 0; i < N; i++)
    Xs += i;
  if (X != Xs)
    abort ();

  if (Y != N)
    abort ();
}

/* Make sure lambda objects do NOT appear in target maps.  */
/* { dg-final { scan-tree-dump {(?n)#pragma omp target num_teams.* map\(from:Y \[len: [0-9]+\]\) map\(from:X \[len: [0-9]+\]\)$} "gimple" } } */
