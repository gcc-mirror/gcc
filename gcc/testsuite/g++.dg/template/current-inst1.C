// PR c++/53549

template<typename T>
struct C2
{
  int operator()();

  template<int> struct F2;
};


template<typename T>
template<int I>
struct C2<T>::F2 : C2<T>
{
  using C2<T>::operator();
};

C2<int>::F2<42> f;
