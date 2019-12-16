// PR c++/92869
// { dg-do compile { target c++11 } }

struct A {
  A () = default;
  A (const A &) = default;
  A (A &&) = default;
  int arr[3];
};

template <typename T, int N>
struct B {
  B () = default;
  B (const B &) = default;
  B (B &&) = default;
  T arr[N];
};

A a = { { 1, 2, 3 } };		// { dg-error "could not convert" "" { target c++2a } }
B<int, 3> b = { { 1, 2, 3 } };	// { dg-error "could not convert" "" { target c++2a } }
