// Test that UTF-8 string literals have type const char8_t[] if -fchar8_t is enabled.
// { dg-do compile { target c++11 } }
// { dg-options "-fchar8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };
template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

static_assert(is_same<decltype(u8""), const char8_t(&)[1]>::value, "Error");
