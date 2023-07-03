// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

// "omp declare mapper" support -- check expansion in gimple.

struct S {
  int *ptr;
  int size;
};

#define N 64

#pragma omp declare mapper (S w) map(w.size, w.ptr, w.ptr[:w.size])
#pragma omp declare mapper (foo:S w) map(to:w.size, w.ptr) map(w.ptr[:w.size])

int main (int argc, char *argv[])
{
  S s;
  s.ptr = new int[N];
  s.size = N;

#pragma omp declare mapper (bar:S w) map(w.size, w.ptr, w.ptr[:w.size])

#pragma omp target
  {
    for (int i = 0; i < N; i++)
      s.ptr[i]++;
  }

#pragma omp target map(tofrom: s)
  {
    for (int i = 0; i < N; i++)
      s.ptr[i]++;
  }

#pragma omp target map(mapper(default), tofrom: s)
  {
    for (int i = 0; i < N; i++)
      s.ptr[i]++;
  }

#pragma omp target map(mapper(foo), alloc: s)
  {
    for (int i = 0; i < N; i++)
      s.ptr[i]++;
  }

#pragma omp target map(mapper(bar), tofrom: s)
  {
    for (int i = 0; i < N; i++)
      s.ptr[i]++;
  }

  return 0;
}

// { dg-final { scan-tree-dump-times {map\(struct:s \[len: 2\]\) map\(alloc:s\.ptr \[len: [0-9]+\]\) map\(tofrom:s\.size \[len: [0-9]+\]\) map\(tofrom:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:s\.ptr \[bias: 0\]\)} 4 "gimple" } }
// { dg-final { scan-tree-dump-times {map\(struct:s \[len: 2\]\) map\(alloc:s\.ptr \[len: [0-9]+\]\) map\(alloc:s\.size \[len: [0-9]+\]\) map\(alloc:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:s\.ptr \[bias: 0\]\)} 1 "gimple" } }
