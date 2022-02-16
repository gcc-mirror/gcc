/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

struct Foo {
  float *a;
  void init(int N) {
    a = new float[N];
    #pragma acc enter data create(a[0:N])
  }
};
int main() { Foo x; x.init(1024); }

/* { dg-final { scan-tree-dump {struct:\*\(struct Foo \*\) this \[len: 1\]\) map\(alloc:this->a \[len: [0-9]+\]\) map\(alloc:\*_[0-9]+ \[len: _[0-9]+\]\) map\(attach:this->a \[bias: 0\]\)} "gimple" } } */
