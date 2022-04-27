// PR c++/104470
// { dg-do compile { target c++20 } }

template<typename _Types>
class variant
{
  template<typename _Tp>
    static constexpr int __accepted_index = 0;
  template<int _Np>
    using __to_type = int;
  template<typename _Tp>
    using __accepted_type = __to_type<__accepted_index<_Tp>>;
  template<typename _Tp, typename _Tj = __accepted_type<_Tp>>
    variant(_Tp __t)  { }
};
template <typename T>
struct Foo
{
  T value;
};
template <typename T>
using V = variant<Foo<T>>;
V e = Foo{1};			// { dg-error "" }
