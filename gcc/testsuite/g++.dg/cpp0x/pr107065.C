// PR c++/107065
// { dg-do compile { target c++11 } }

template<class, class> struct is_same { static constexpr bool value = false; };
template<class T> struct is_same<T, T> { static constexpr bool value = true; };

int
main ()
{
  bool b = true;
  static_assert (is_same<decltype (!(!b)), bool>::value, "");
  auto bb = (!(!b));
  static_assert (is_same<decltype (bb), bool>::value, "");
}
