// { dg-do assemble  }
// Origin: bitti@cs.tut.fi

template<typename T, unsigned int N>
class Vector
{
public:
  template<unsigned int I>
  class Vector<T,N>::CommaInit { }; // { dg-error "" } invalid definition
};
