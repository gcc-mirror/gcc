// Test that char is deduced for UTF-8 character and string literals when
// -fchar8_t is not in effect.
// { dg-do compile }
// { dg-options "-std=c++17 -fno-char8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };

template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

template<typename T1, typename T2, typename T3>
void ft(T1, T2, T3 &) {
  static_assert(is_same<T1, char>::value, "Error");
  static_assert(is_same<T2, const char*>::value, "Error");
  static_assert(is_same<T3, const char[2]>::value, "Error");
}

auto x = (ft(u8'x', u8"x", u8"x"),0);

auto c8 = u8'x';
static_assert(is_same<decltype(c8), char>::value, "Error");

auto c8p = u8"x";
static_assert(is_same<decltype(c8p), const char*>::value, "Error");

auto &c8a = u8"x";
static_assert(is_same<decltype(c8a), const char(&)[2]>::value, "Error");
