// DR 1670 - auto as conversion-type-id
// { dg-do compile { target c++14 } }

struct S {
  operator auto () { return 0; }		// { dg-error "invalid use of 'auto' in conversion operator" }
};
struct T {
  operator decltype (auto) () { return 0; }	// { dg-error "invalid use of 'auto' in conversion operator" }
};
