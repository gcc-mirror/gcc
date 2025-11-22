// PR c++/122185
// { dg-do compile { target c++17 } }
// { dg-options "-w -O2" }

namespace std {
template <typename _Tp> struct remove_reference {
  using type = __remove_reference(_Tp);
};
template <typename _Tp> _Tp forward(typename remove_reference<_Tp>::type);
}
template <typename T> struct static_const {
  static constexpr T value{};
};
template <typename, typename> struct adl_serializer;
template <template <typename, typename> class = adl_serializer>
class basic_json;
struct from_json_fn {
  template <typename BasicJsonType, typename T>
  void operator()(BasicJsonType, T) const;
};
template <typename, typename> struct adl_serializer {
  template <typename BasicJsonType, typename TargetType>
  static void from_json(BasicJsonType &&j, TargetType val) {
    static_const<from_json_fn>::value(std::forward<BasicJsonType>(j), val);
  }
};
struct iter_impl {
  basic_json<> operator*();
  void operator++();
  bool operator!=(iter_impl);
};
template <template <typename, typename = void> class JSONSerializer>
struct basic_json {
  template <typename> using iter_impl = iter_impl;
  using iterator = iter_impl<basic_json>;
  template <typename> void get_impl(int) {
    auto ret = int();
    JSONSerializer<int>::from_json(*this, ret);
  }
  template <typename> void get() { get_impl<int>({}); }
  iterator begin();
  iterator end();
  char type_name;
};
void _ValidateSignatureFile() {
  basic_json signatures;
  for (auto signature : signatures)
    signature.get<int>();
}
