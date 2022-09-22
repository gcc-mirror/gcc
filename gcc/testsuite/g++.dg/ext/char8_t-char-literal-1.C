// Test that UTF-8 character literals have type char if -fchar8_t is not enabled.
// { dg-do compile }
// { dg-options "-std=c++17 -fsigned-char -fno-char8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };
template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

static_assert(is_same<decltype(u8'x'), char>::value, "Error");

#if u8'\0' - 1 > 0
#error "UTF-8 character literals not signed in preprocessor"
#endif
