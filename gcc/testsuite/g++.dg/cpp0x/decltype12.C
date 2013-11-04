// { dg-do compile }
// { dg-options "-std=c++11" }
template<typename T, typename U>
struct is_same
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

int&& f(const int&) {}
int&& (*fp)(const int&) = f;
int&& (&fr)(const int&) = f;

struct X { int&& f(const int&); };

int&& (X::*mfp)(const int&) = &X::f;

void g(X& xr, X* xp)
{
  int i;
  static_assert(is_same<decltype(f(i)), int&&>::value, "direct call");
  static_assert(is_same<decltype(fp(i)), int&&>::value, "pointer");
  static_assert(is_same<decltype((*fp)(i)), int&&>::value, 
                "dereferenced pointer");
  static_assert(is_same<decltype(fr(i)), int&&>::value, 
                "reference");
  static_assert(is_same<decltype(xr.f(i)), int&&>::value,
                "member function call");
  static_assert(is_same<decltype((xr.*mfp)(i)), int&&>::value, 
                "member function pointer with .*");
  static_assert(is_same<decltype((xp->*mfp)(i)), int&&>::value, 
                "member function pointer with ->*");
}
