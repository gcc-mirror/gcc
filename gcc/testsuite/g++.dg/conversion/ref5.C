// PR c++/96104

template <typename T> void fn(T &);
class E {};
struct F {
  template <typename T> void mfn(T t) { t, fn(E()); } // { dg-error "cannot bind non-const lvalue reference" }
};
int
main()
{
  E e;
  F f;
  f.mfn(e);
}
