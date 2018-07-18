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
  W(W&) = default;
  // T1 and T2 may have differing ref-qualifiers (copy assign op).
  constexpr W& operator=(const W&) && = default; // { dg-error "defaulted" "" { target c++11_down } }
  T t;
};

W<M> w;
