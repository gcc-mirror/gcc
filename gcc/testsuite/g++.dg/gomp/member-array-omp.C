/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

struct Foo {
  float *a;
  void init(int N) {
    a = new float[N];
    #pragma omp target enter data map(alloc:a[0:N])
  }
};
int main() { Foo x; x.init(1024); }

/* { dg-final { scan-tree-dump {map\(alloc:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:this->a \[bias: 0\]\)} "gimple" } } */
