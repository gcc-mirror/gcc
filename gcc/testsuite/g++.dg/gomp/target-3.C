// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-gimple" }

struct S
{
  int a, b;
  void bar (int);
};

void
S::bar (int x)
{
  #pragma omp target map (alloc: a, b)
    ;
  #pragma omp target enter data map (alloc: a, b)
}

template <int N>
struct T
{
  int a, b;
  void bar (int);
};

template <int N>
void
T<N>::bar (int x)
{
  #pragma omp target map (alloc: a, b)
    ;
  #pragma omp target enter data map (alloc: a, b)
}

template struct T<0>;

/* { dg-final { scan-tree-dump-times "map\\(struct:\\*\\(struct S \\*\\) this \\\[len: 2\\\]\\) map\\(alloc:this->a \\\[len: \[0-9\]+\\\]\\) map\\(alloc:this->b \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */

/* { dg-final { scan-tree-dump-times "map\\(struct:\\*\\(struct T \\*\\) this \\\[len: 2\\\]\\) map\\(alloc:this->a \\\[len: \[0-9\]+\\\]\\) map\\(alloc:this->b \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
