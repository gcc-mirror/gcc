// PR c++/9907
// Origin: nes@lrde.epita.fr
// sizeof(foo()) was not considered constant.


template <unsigned n> struct bar {};

int foo();

template <class T>
void baz()
{
  bar<sizeof(foo())> b;
}

