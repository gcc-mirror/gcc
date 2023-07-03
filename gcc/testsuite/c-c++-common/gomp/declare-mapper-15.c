/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

typedef struct {
  int a, b, c, d;
} S;

int main ()
{
  S s;
  #pragma omp declare mapper (S x) map(alloc: x.a) map(to: x.b) \
				   map(from: x.c) map(tofrom: x.d)

  #pragma omp target enter data map(to: s)

  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(alloc:s\.a \[len: [0-9]+\]\) map\(to:s\.b \[len: [0-9]+\]\) map\(alloc:s\.c \[len: [0-9]+\]\) map\(to:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */

  #pragma omp target exit data map(from: s)

  /* { dg-final { scan-tree-dump-times {map\(release:s\.a \[len: 4\]\) map\(release:s\.b \[len: [0-9]+\]\) map\(from:s\.c \[len: [0-9]+\]\) map\(from:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */


  #pragma omp target enter data map(alloc: s)

  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(alloc:s\.a \[len: [0-9]+\]\) map\(alloc:s\.b \[len: [0-9]+\]\) map\(alloc:s\.c \[len: [0-9]+\]\) map\(alloc:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */

  #pragma omp target exit data map(release: s)

  /* { dg-final { scan-tree-dump-times {map\(release:s\.a \[len: [0-9]+\]\) map\(release:s\.b \[len: [0-9]+\]\) map\(release:s\.c \[len: [0-9]+\]\) map\(release:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */


  #pragma omp target enter data map(present, to: s)

  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(force_present:s\.a \[len: [0-9]+\]\) map\(force_present:s\.b \[len: [0-9]+\]\) map\(force_present:s\.c \[len: [0-9]+\]\) map\(force_present:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */

  #pragma omp target exit data map(present, from: s)

  /* { dg-final { scan-tree-dump-times {map\(release:s\.a \[len: [0-9]+\]\) map\(release:s\.b \[len: [0-9]+\]\) map\(force_present:s\.c \[len: [0-9]+\]\) map\(force_present:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */


  #pragma omp target enter data map(always, to: s)

  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(alloc:s\.a \[len: [0-9]+\]\) map\(always,to:s\.b \[len: [0-9]+\]\) map\(alloc:s\.c \[len: [0-9]+\]\) map\(always,to:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */

  #pragma omp target exit data map(always, from: s)

  /* { dg-final { scan-tree-dump-times {map\(release:s\.a \[len: [0-9]+\]\) map\(release:s\.b \[len: [0-9]+\]\) map\(always,from:s\.c \[len: [0-9]+\]\) map\(always,from:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */


  #pragma omp target enter data map(always, present, to: s)

  /* { dg-final { scan-tree-dump-times {map\(struct:s \[len: 4\]\) map\(force_present:s\.a \[len: [0-9]+\]\) map\(always,present,to:s\.b \[len: [0-9]+\]\) map\(force_present:s\.c \[len: [0-9]+\]\) map\(always,present,to:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */

  #pragma omp target exit data map(always, present, from: s)

  /* { dg-final { scan-tree-dump-times {map\(release:s\.a \[len: [0-9]+\]\) map\(release:s\.b \[len: [0-9]+\]\) map\(always,present,from:s\.c \[len: [0-9]+\]\) map\(always,present,from:s\.d \[len: [0-9]+\]\)} 1 "gimple" } } */

  return 0;
}
