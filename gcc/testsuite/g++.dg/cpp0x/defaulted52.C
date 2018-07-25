// Core Issue #1331 (const mismatch with defaulted copy constructor)
// { dg-do compile { target c++11 } }

struct M
{
  M();
  // So that W wouldn't have had "const W&" copy ctor if it were
  // implicitly declared.
  M(M&);
};

template<typename T> struct W
{
  W();
  // This should now compile and be =deleted.
  W(const W&) = default;
  T t;
};

W<M> w;
