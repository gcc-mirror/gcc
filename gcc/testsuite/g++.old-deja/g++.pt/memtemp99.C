// Build don't link:
// Origin: bitti@cs.tut.fi

template<typename T, unsigned int N>
class Vector
{
public:
  template<unsigned int I>
  class Vector<T,N>::CommaInit { }; // ERROR - invalid definition
};
