// PR c++/81247 rejected well-formed

namespace N {
  template < typename T > class A
  {
    // injects a hidden class N::N at instantiation time
    template < T > friend class N;
  };
}

void f ()
{
  N::A < int > a1;
}
