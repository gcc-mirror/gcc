// PR c++/48280
// { dg-options -std=c++11 }

struct S {
  template < typename > S (const S &) = default; // { dg-error "" }
};
