// { dg-options "-std=c++0x" }

template <unsigned Len, unsigned Align>
struct aligned_storage
{
    using type __attribute__((aligned((Align)))) =
        char[Len];
};

template<typename T>
struct X
{
  typename aligned_storage<sizeof(T),__alignof(T)>::type data;
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof (X<double>) == __alignof (double)> dummy;
