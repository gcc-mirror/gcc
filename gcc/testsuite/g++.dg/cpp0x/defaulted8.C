// PR c++/38649
// { dg-options "-std=c++0x" }

struct A
{
  A(...) = default;		// { dg-error "cannot be defaulted" }
  A(const A&, ...) = default;	// { dg-error "cannot be defaulted" }
};
