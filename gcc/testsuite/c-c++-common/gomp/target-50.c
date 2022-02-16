/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

typedef struct
{
  int *arr;
} L;

int main()
{
  L *tmp;

  /* There shouldn't be an order dependency here...  */

  #pragma omp target map(to: tmp->arr) map(tofrom: tmp->arr[0:10])
  { }

  #pragma omp target map(tofrom: tmp->arr[0:10]) map(to: tmp->arr)
  { }
/* { dg-final { scan-tree-dump-times {map\(struct:\*tmp \[len: 1\]\) map\(alloc:tmp[._0-9]*->arr \[len: [0-9]+\]\) map\(tofrom:\*_[0-9]+ \[len: [0-9]+\]\) map\(attach:tmp[._0-9]*->arr \[bias: 0\]\)} 2 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } } */

  return 0;
}
