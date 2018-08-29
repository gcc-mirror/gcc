// PR c++/85242

namespace N
{
  struct A {};
}

template<struct N::A {}> void foo(); // { dg-error "" }
