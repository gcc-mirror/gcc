// Test that UTF-8 character literals have type char8_t if -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-std=c++17 -fchar8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };
template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

static_assert(is_same<decltype(u8'x'), char8_t>::value, "Error");
