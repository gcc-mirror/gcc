// PR c++/93551
// { dg-do compile { target concepts } }

namespace std {
  template<typename _Tp, _Tp __v>
  struct integral_constant
  {
    static constexpr _Tp                  value = __v;
    typedef _Tp                           value_type;
    typedef integral_constant<_Tp, __v>   type;
    constexpr operator value_type() const noexcept { return value; }
  };
  template<typename _Base, typename _Derived>
  struct is_base_of
    : public integral_constant<bool, __is_base_of(_Base, _Derived)>
  { };
  template <typename _Base, typename _Derived>
  inline constexpr bool is_base_of_v = is_base_of<_Base, _Derived>::value;
}
class Bar { };
struct Foo {
  template <typename P> requires std::is_base_of_v<Bar, P>
  Foo(P const&);
};
template <typename P>
Foo fun(P const& arg) {
  (bool)arg;			// { dg-error "" }
  return Foo {arg};
}
int main() {
  fun(Bar{});
  return 0;
}
