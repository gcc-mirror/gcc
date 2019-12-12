// Test overloading for UTF-8 user defined literals when -fchar8_t is not in effect.
// { dg-do compile }
// { dg-options "-std=c++17 -fno-char8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };

template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

int operator "" _udcl(char);
static_assert(is_same<decltype('x'_udcl), int>::value, "Error");
static_assert(is_same<decltype(u8'x'_udcl), int>::value, "Error");

int operator "" _udsl(const char*, __SIZE_TYPE__);
static_assert(is_same<decltype("x"_udsl), int>::value, "Error");
static_assert(is_same<decltype(u8"x"_udsl), int>::value, "Error");
