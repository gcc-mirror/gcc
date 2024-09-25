// Core Issue #1331 (const mismatch with defaulted copy constructor)
// { dg-do compile { target c++11 } }

struct M
{
  M();
  M(M&);
};

template<typename T> struct W
{
  W();
  W(const W&) = default; // { dg-error "binding" }
// { dg-error "implicitly deleted" "" { target c++17_down } .-1 }
  T t;
};

W<M> w;
W<M> w2 = w; // { dg-error "use of deleted function" }
