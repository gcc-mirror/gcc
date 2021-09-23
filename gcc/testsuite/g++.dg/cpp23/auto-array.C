// PR c++/100975
// DR 2397 - auto specifier for pointers and references to arrays 
// { dg-do compile { target c++11 } }

struct false_type { static constexpr bool value = false; };
struct true_type { static constexpr bool value = true; };
template<class T, class U>
struct is_same : false_type {}; 
template<class T>
struct is_same<T, T> : true_type {};

using U = int[3];

void
g ()
{
  int a[3];
  auto (*p)[3] = &a;
  auto (&r)[3] = a;
  int aa[3][3];
  auto (*pp)[3][3] = &aa;
  auto (&rr)[3][3] = aa;

  auto (&&rv)[3] = U{};

  static_assert (is_same<decltype (p), int(*)[3]>::value, "");
  static_assert (is_same<decltype (pp), int(*)[3][3]>::value, "");
  static_assert (is_same<decltype (r), int(&)[3]>::value, "");
  static_assert (is_same<decltype (rv), int(&&)[3]>::value, "");
  static_assert (is_same<decltype (rr), int(&)[3][3]>::value, "");

#if __cplusplus >= 201402L
  // In a generic lambda parameter this was OK even before.
  auto l = [](auto (&arr)[5]) { return arr[0]; };
#endif
}
