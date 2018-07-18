// PR c++/68884
// { dg-do compile { target c++11 } }

namespace std {
  template <typename _Tp, _Tp __v> struct A { static constexpr _Tp value = __v; };
typedef A<bool, true> true_type;
}
template <int> struct VsA;
template <class ValueType> struct ValueTemplate {
  template <template <ValueType> class, class> struct IsInstanceOf;
  template <template <ValueType> class TemplateA, ValueType... TypesA>
  struct IsInstanceOf<TemplateA, TemplateA<TypesA...>> : std::true_type {};
};
bool foo = ValueTemplate<int>::IsInstanceOf<VsA, VsA<0>>::value;
