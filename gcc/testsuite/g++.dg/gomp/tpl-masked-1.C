// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-gimple" }

int i;

template <typename T> void f1 (bool p, T t)
{
  if (p)
    {
      #pragma omp masked filter (t)
	i++;
    }
}

void f2 ()
{
  f1<int> (true, 0);
  f1<long> (true, 0L);
}

// { dg-final { scan-tree-dump-times "#pragma omp masked" 2 "gimple" } }
