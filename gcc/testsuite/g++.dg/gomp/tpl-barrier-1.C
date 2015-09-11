// PR c++/24735
// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-gimple" }

template <int> void f1 ()
{
  #pragma omp barrier
}

template <int> void f2 (bool p)
{
  if (p)
    {
      #pragma omp barrier
    }
}

void f3 ()
{
  f1<0> ();
  f2<0> (true);
}

// { dg-final { scan-tree-dump-times "GOMP_barrier" 2 "gimple" } }
