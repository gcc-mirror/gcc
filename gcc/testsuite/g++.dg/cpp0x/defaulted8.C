// PR c++/38649
// { dg-do compile { target c++11 } }

struct A
{
  A(...) = default;		// { dg-error "cannot be defaulted" }
  A(const A&, ...) = default;	// { dg-error "cannot be defaulted" }
};
