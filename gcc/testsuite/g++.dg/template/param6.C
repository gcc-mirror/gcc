// PR c++/61355
// Verify we perform array-to-pointer and function-to-pointer conversion
// on the substituted/deduced type of an NTTP.

int f();
int p[5];

namespace cpp98 {
  template<class T, T> struct X;
  typedef X<int(), f> ty1;
  typedef X<int[5], p> ty2;
}

namespace cpp11 {
#if __cpp_variadic_templates
  template<class T, T...> struct X;
  using ty1 = X<int(), f>;
  using ty2 = X<int[5], p>;
#endif
}

namespace cpp17 {
#if __cpp_nontype_template_parameter_auto
  template<decltype(auto)> struct X;
  using ty1 = X<f>;
  using ty2 = X<p>;

  template<decltype(auto)...> struct Y;
  using ty3 = Y<f>;
  using ty4 = Y<p>;
#endif
}
