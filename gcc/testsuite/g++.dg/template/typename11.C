// PR c++/28999

namespace N
{
  template<int> void foo();
}

template<int> struct A
{
  friend void typename N::foo<0>(); // { dg-error "type|expected" }
};
