// DR 1670 - auto as conversion-type-id
// { dg-do compile { target c++14 } }
// { dg-options "-Wpedantic" }

struct S {
  operator auto () { return 0; }		// { dg-warning "invalid use of 'auto' in conversion operator" }
};
struct T {
  operator decltype (auto) () { return 0; }	// { dg-warning "invalid use of 'auto' in conversion operator" }
};
