// PR c++/85634 ICE managing lookup set

namespace N {
  template <class T> void Foo (T *const &);
}

using namespace N;

template<class T> void Foo (const T &);


template <class T>
void Frob()
{
  void (*op)(const T&) = Foo;
}

template void Frob<int *>();
