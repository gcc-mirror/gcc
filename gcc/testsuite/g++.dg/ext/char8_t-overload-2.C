// Test overloading for UTF-8 literals when -fchar8_t is in effect.
// { dg-do compile }
// { dg-options "-std=c++17 -fchar8_t" }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };

template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

int fc(char);
long fc(char8_t);
static_assert(is_same<decltype(fc('x')), int>::value, "Error");
static_assert(is_same<decltype(fc(u8'x')), long>::value, "Error");

int fs(const char*);
long fs(const char8_t*);
static_assert(is_same<decltype(fs("x")), int>::value, "Error");
static_assert(is_same<decltype(fs(u8"x")), long>::value, "Error");

int fr(const char(&)[2]);
long fr(const char8_t(&)[2]);
static_assert(is_same<decltype(fr("x")), int>::value, "Error");
static_assert(is_same<decltype(fr(u8"x")), long>::value, "Error");
