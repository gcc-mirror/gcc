// { dg-do compile { target c++ } }
// { dg-additional-options "-fdump-tree-gimple" }

#include <stdlib.h>

// Test named mapper invocation.

struct S {
  int *ptr;
  int size;
};

int main (int argc, char *argv[])
{
  int N = 1024;
#pragma omp declare mapper (mapN:struct S s) map(to:s.ptr, s.size) \
					     map(s.ptr[:N])

  struct S s;
  s.ptr = (int *) malloc (sizeof (int) * N);

#pragma omp target map(mapper(mapN), tofrom: s)
// { dg-final { scan-tree-dump {map\(struct:s \[len: 2\]\) map\(alloc:s\.ptr \[len: [0-9]+\]\) map\(to:s\.size \[len: [0-9]+\]\) map\(tofrom:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:s\.ptr \[bias: 0\]\)} "gimple" } }
  {
    for (int i = 0; i < N; i++)
      s.ptr[i]++;
  }

  return 0;
}
