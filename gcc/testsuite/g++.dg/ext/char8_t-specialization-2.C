// Test specialization for UTF-8 literals when -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-std=c++17 -fchar8_t" }

template<auto> struct ct { static constexpr int dm = 1; };
template<> struct ct<'x'> { static constexpr int dm = 2; };
template<> struct ct<u8'x'> { static constexpr int dm = 3; };
static_assert(ct<'x'>::dm == 2, "Error");
static_assert(ct<u8'x'>::dm == 3, "Error");

template<typename T, const T *> struct ct2 { static constexpr int dm = 4; };
template<const char *P> struct ct2<char,P> { static constexpr int dm = 5; };
template<const char8_t *P> struct ct2<char8_t,P> { static constexpr int dm = 6; };
constexpr const char s[] = "x";
constexpr const char8_t s8[] = u8"x";
static_assert(ct2<char,s>::dm == 5, "Error");
static_assert(ct2<char8_t,s8>::dm == 6, "Error");
