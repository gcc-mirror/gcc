// Test specialization for UTF-8 literals when -fchar8_t is not enabled.
// { dg-do compile }
// { dg-options "-std=c++17 -fno-char8_t" }

template<auto> struct ct { static constexpr int dm = 1; };
template<> struct ct<'x'> { static constexpr int dm = 2; };
static_assert(ct<'x'>::dm == 2, "Error");
static_assert(ct<u8'x'>::dm == 2, "Error");
