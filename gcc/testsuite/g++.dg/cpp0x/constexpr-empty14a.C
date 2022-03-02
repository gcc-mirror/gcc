// PR c++/101072
// { dg-do compile { target c++11 } }
// { dg-additional-options -fno-elide-constructors }

struct S {};

template <class T> void
foo (S s)
{
  constexpr S x = s;
}
