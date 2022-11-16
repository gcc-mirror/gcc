// DR 2428
// { dg-do compile { target c++20 } }

template<typename T>
concept C1 [[deprecated]] = true;

template<typename T>
concept C2 __attribute__((deprecated)) = false;

template<typename T>
concept C3 [[deprecated]] = true;

template<typename T>
concept C4 __attribute__((deprecated)) = false;

static_assert(C3<int>);	// { dg-warning "'C3' is deprecated" }
static_assert(C4<int>); // { dg-error "static assertion failed" }
			// { dg-warning "'C4' is deprecated" "" { target *-*-* } .-1 }

template<typename T>
  requires C3<T>	// { dg-warning "'C3' is deprecated" }
int fn1(T t) { return 0; }
