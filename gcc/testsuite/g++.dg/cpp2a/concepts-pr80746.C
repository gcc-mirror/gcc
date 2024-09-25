// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<typename T, typename T::type>
concept C = true;

template<C<0> T> class ct {};

struct S
{
  using type = int;
};

template class ct<S>;
