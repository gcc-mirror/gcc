// PR c++/34267
// { dg-do compile }

struct A {};
__decltype (A);		// { dg-error "must be an expression" }
template<int> struct B
{
  __decltype (A);	// { dg-error "must be an expression" }
  __decltype (~A);	// { dg-error "must be an expression" }
  __decltype (B);	// { dg-error "must be an expression" }
  __decltype (~B);	// { dg-error "must be an expression" }
};
