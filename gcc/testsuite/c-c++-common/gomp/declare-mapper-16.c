/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

typedef struct {
  int a, b, c, d;
} S;

int main ()
{
  S s = { 0, 0, 0, 0 };
  #pragma omp declare mapper (S x) map(alloc: x.a) map(to: x.b) \
				   map(from: x.c) map(tofrom: x.d)

  #pragma omp target data map(s)
  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(alloc:s\.a \[len: [0-9]+\]\) map\(to:s\.b \[len: [0-9]+\]\) map\(from:s\.c \[len: [0-9]+\]\) map\(tofrom:s\.d \[len: [0-9]+\]\)} 3 "gimple" } } */
  {
    #pragma omp target
    {
      s.a++;
      s.b++;
      s.c++;
      s.d++;
    }
  }

  #pragma omp target data map(alloc: s)
  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(alloc:s\.a \[len: [0-9]+\]\) map\(alloc:s\.b \[len: [0-9]+\]\) map\(alloc:s\.c \[len: [0-9]+\]\) map\(alloc:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */
  {
    #pragma omp target
    {
      s.a++;
      s.b++;
      s.c++;
      s.d++;
    }
  }

  return 0;
}
