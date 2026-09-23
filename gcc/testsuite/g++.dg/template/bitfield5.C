// PR c++/125674
// { dg-do compile { target c++20 } }

struct A { int a : 1.; };	// { dg-error "width of bit-field 'a' has non-integral type 'double'" }
template <typename T>
struct B { int b : 1.; };	// { dg-error "width of bit-field 'b' has non-integral type 'double'" }
template <typename T>
struct C { T c : 1.; };		// { dg-error "width of bit-field 'c' has non-integral type 'double'" }
template <auto N>
struct D { int d : N; };	// { dg-error "width of bit-field 'D<N>::d' has non-integral type 'double'" }
template <typename T, auto N>
struct E { T e : N; };		// { dg-error "width of bit-field 'E<T, N>::e' has non-integral type 'double'" }
B <int> b;
C <int> c;
D <1.> d;
E <int, 1.> e;
