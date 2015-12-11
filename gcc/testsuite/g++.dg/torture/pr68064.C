// { dg-do compile }

template <class Config> class A {
public:
  class B;
  typedef typename Config::template D<A>::type TypeHandle;
  static A *Tagged() { return B::New(B::kTagged); }
  static TypeHandle Union(TypeHandle);
  static TypeHandle Representation(TypeHandle, typename Config::Region *);
  bool Is();
};

template <class Config> class A<Config>::B {
  friend A;
  enum { kTaggedPointer = 1 << 31, kTagged = kTaggedPointer };
  static A *New(int p1) { return Config::from_bitset(p1); }
};

struct C {
  typedef int Region;
  template <class> struct D { typedef A<C> *type; };
  static A<C> *from_bitset(unsigned);
};
A<C> *C::from_bitset(unsigned p1) { return reinterpret_cast<A<C> *>(p1); }

namespace {
int *a;
void fn1(A<C> *p1) { A<C>::Union(A<C>::Representation(p1, a)); }
}

void fn2() {
  A<C> b;
  A<C> *c = b.Is() ? 0 : A<C>::Tagged();
  fn1(c);
}
