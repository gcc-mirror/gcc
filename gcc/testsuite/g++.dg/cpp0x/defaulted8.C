// PR c++/38649
// { dg-options "-std=c++11" }

struct A
{
  A(...) = default;		// { dg-error "cannot be defaulted" }
  A(const A&, ...) = default;	// { dg-error "cannot be defaulted" }
};
