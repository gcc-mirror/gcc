/* { dg-options "-O1 -fgraphite-identity -fno-loop-block -fno-loop-interchange -fno-loop-strip-mine" } */

typedef __SIZE_TYPE__ size_t;
inline void* operator new(size_t, void* __p) throw() { return __p; }

struct A {
  int i, j;
  A() : i(0) {}
};

void Init(A *a)
{
  for (int i = 0; i < 20; i++) {
    new (&a[i]) A;
    a[i].j = 0;
  }
}
