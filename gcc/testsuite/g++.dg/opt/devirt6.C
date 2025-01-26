// { dg-do compile { target c++11 } }
// { dg-options "-O3" }

// PR tree-optimization/118077

// This used to ICE because the devirtualization call
// of bb inside f1 (which was inlined into f2) became
// a direct call to c1::bb but the abnormal edge
// was not removed even though bb was const.

int f() __attribute__((returns_twice));
struct c1 {
  virtual int bb(void) const { return 0; }
  bool f1(int a)
  {
    return a && !bb();
  }
};
struct c2 final : c1 { void f2(int); };
void c2::f2(int a) {
  if (!f1(a))
    f();
}
