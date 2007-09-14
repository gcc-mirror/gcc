// PR c++/17743

template <unsigned Len, unsigned Align>
struct aligned_storage
{
  typedef char type[Len] __attribute__((aligned((Align))));
};

template<typename T>
struct X
{
  typename aligned_storage<sizeof(T),__alignof(T)>::type data;
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof (X<double>) == __alignof (double)> dummy;
