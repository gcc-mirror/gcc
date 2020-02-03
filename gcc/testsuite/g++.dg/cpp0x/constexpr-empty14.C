// PR c++/91953
// { dg-do compile { target c++11 } }

struct S {};

template <class T> void
foo (S s)
{
  constexpr S x = s;
}
