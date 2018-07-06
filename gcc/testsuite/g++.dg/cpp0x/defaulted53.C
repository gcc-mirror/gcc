// Core Issue #1331 (const mismatch with defaulted copy constructor)
// { dg-do compile { target c++11 } }

struct M
{
  M& operator=(M&);
};

struct R
{
  R& operator=(R&) = default;
  M m;
};

struct S
{
  S& operator=(const S&) = default;
  M m;
};

struct T
{
  // If F is an assignment operator, and the return type of T1
  // differs from the return type of T2 the program is ill-formed.
  T operator=(T&) = default; // { dg-error "defaulted" }
  M m;
};

struct U
{
  // If F is an assignment operator, and T1's parameter type is
  // not a reference, the program is ill-formed.
  U& operator=(U) = default; // { dg-error "defaulted" }
  M m;
};
