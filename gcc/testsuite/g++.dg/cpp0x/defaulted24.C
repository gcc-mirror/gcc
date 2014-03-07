// PR c++/48280
// { dg-do compile { target c++11 } }

struct S {
  template < typename > S (const S &) = default; // { dg-error "" }
};
