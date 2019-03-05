// Test that UTF-8 string literals have type const char[] if -fchar8_t is not enabled.
// { dg-do compile { target c++11 } }
// { dg-options "-fno-char8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };
template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

static_assert(is_same<decltype(u8""), const char(&)[1]>::value, "Error");
