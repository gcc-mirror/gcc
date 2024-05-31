// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Make sure that we check partial concept ids
// with variable concepts.

template<class A, class B>
concept C = true;

template<C<int> D>
struct E
{
  int f = 0;
};

E<double> e;
