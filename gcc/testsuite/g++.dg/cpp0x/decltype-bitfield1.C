// PR c++/95009
// { dg-do compile { target { c++11 && longlong64 } } }

struct false_type { static constexpr bool value = false; };
struct true_type { static constexpr bool value = true; };
template<class T, class U>
struct is_same : false_type {}; 
template<class T>
struct is_same<T, T> : true_type {};

struct A {
  int i : 31;
  unsigned long long l : 37;
} a;

void
g ()
{
  // Careful: pre{in,de}crements are lvalues -> deduce T&.  */
  static_assert (is_same<decltype(a.i), int>::value, "");
  static_assert (is_same<decltype((a.i)), int&>::value, "");
  static_assert (is_same<decltype(++a.i), int&>::value, "");
  static_assert (is_same<decltype((++a.i)), int&>::value, "");
  static_assert (is_same<decltype(a.i++), int>::value, "");
  static_assert (is_same<decltype((a.i++)), int>::value, "");
  static_assert (is_same<decltype(--a.i), int&>::value, "");
  static_assert (is_same<decltype((--a.i)), int&>::value, "");
  static_assert (is_same<decltype(a.i--), int>::value, "");
  static_assert (is_same<decltype((a.i--)), int>::value, "");
  static_assert (is_same<decltype(a.i += 1), int&>::value, "");
  static_assert (is_same<decltype((a.i += 1)), int&>::value, "");
  static_assert (is_same<decltype(a.i -= 1), int&>::value, "");
  static_assert (is_same<decltype((a.i -= 1)), int&>::value, "");
  static_assert (is_same<decltype(a.i *= 1), int&>::value, "");
  static_assert (is_same<decltype((a.i *= 1)), int&>::value, "");
  static_assert (is_same<decltype(+a.i), int>::value, "");
  static_assert (is_same<decltype((+a.i)), int>::value, "");
  static_assert (is_same<decltype(-a.i), int>::value, "");
  static_assert (is_same<decltype((-a.i)), int>::value, "");
  static_assert (is_same<decltype(~a.i), int>::value, "");
  static_assert (is_same<decltype((~a.i)), int>::value, "");

  static_assert (is_same<decltype(a.l), unsigned long long>::value, "");
  static_assert (is_same<decltype((a.l)), unsigned long long&>::value, "");
  static_assert (is_same<decltype(++a.l), unsigned long long&>::value, "");
  static_assert (is_same<decltype((++a.l)), unsigned long long&>::value, "");
  static_assert (is_same<decltype(a.l++), unsigned long long>::value, "");
  static_assert (is_same<decltype((a.l++)), unsigned long long>::value, "");
  static_assert (is_same<decltype(--a.l), unsigned long long&>::value, "");
  static_assert (is_same<decltype((--a.l)), unsigned long long&>::value, "");
  static_assert (is_same<decltype(a.l--), unsigned long long>::value, "");
  static_assert (is_same<decltype((a.l--)), unsigned long long>::value, "");
  static_assert (is_same<decltype(a.l += 1), unsigned long long&>::value, "");
  static_assert (is_same<decltype((a.l += 1)), unsigned long long&>::value, "");
  static_assert (is_same<decltype(a.l -= 1), unsigned long long&>::value, "");
  static_assert (is_same<decltype((a.l -= 1)), unsigned long long&>::value, "");
  static_assert (is_same<decltype(a.l *= 1), unsigned long long&>::value, "");
  static_assert (is_same<decltype((a.l *= 1)), unsigned long long&>::value, "");
  static_assert (is_same<decltype(+a.l), unsigned long long>::value, "");
  static_assert (is_same<decltype((+a.l)), unsigned long long>::value, "");
  static_assert (is_same<decltype(-a.l), unsigned long long>::value, "");
  static_assert (is_same<decltype((-a.l)), unsigned long long>::value, "");
  static_assert (is_same<decltype(~a.l), unsigned long long>::value, "");
  static_assert (is_same<decltype((~a.l)), unsigned long long>::value, "");
}
