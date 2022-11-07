// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <int I> struct B { static const int i = I; };
template <int I> concept bool Few = I < 10;

constexpr int g(B<Few> b); // { dg-error "does not constrain a type|invalid" }

