// PR c++/91962 - ICE with reference binding and qualification conversion.
// { dg-do compile { target c++11 } }

template <typename a> class b {
public:
  void c(const a &);
};
class B {
  void d();
  b<const int *> e;
};
long f;
void B::d() { e.c((const int *)f); }
