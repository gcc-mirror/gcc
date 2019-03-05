// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <int I> struct B { static const int i = I; };
template <int I> concept bool Few = I < 10;

constexpr int g(B<Few> b) { return b.i; }

#define SA(X) static_assert((X),#X)
SA(g(B<2>{}) == 2);
SA(g(B<10>{}) == 10); 		// { dg-error "" }
