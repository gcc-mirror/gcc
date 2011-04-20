// PR c++/48657

template<unsigned> struct A { typedef int T; };

template<unsigned> void f()
{
  const unsigned D = 4;
  A<D>::T t;
}
