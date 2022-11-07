// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

// Make sure that we check partial concept ids
// with variable concepts.

template<class A, class B>
concept bool C = true;

template<C<int> D>
struct E
{
  int f = 0;
};

E<double> e;
