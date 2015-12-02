// PR c++/68653
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

struct B;
struct A {
  template <typename T> __attribute__((nonnull)) bool foo (int T::*);
  void bar (B *);
};

template <typename T> bool A::foo (int T::*p)
{
  return p;
}
void A::bar (B *)
{
  foo ((int B::*) nullptr);
}
// { dg-warning "nonnull argument" "" {target "*-*-*"} 0 }
