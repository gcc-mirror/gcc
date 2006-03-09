// PR c++/24734
// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-gimple" }

int i;

template <int> void f1 ()
{
  #pragma omp ordered
    i++;
}

template <int> void f2 (bool p)
{
  if (p)
    {
      #pragma omp master
	i++;
    }
}

void f3 ()
{
  f1<0> ();
  f2<0> (true);
}

// { dg-final { scan-tree-dump-times "#pragma omp ordered" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "#pragma omp master" 1 "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }
