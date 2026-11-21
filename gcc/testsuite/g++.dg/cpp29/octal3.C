// P0085R5 - Oo... adding a coherent character sequence to begin octal-literals
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

template <typename T, typename U>
struct is_same { static constexpr bool value = false; };
template <typename T>
struct is_same <T, T> { static constexpr bool value = true; };
auto a = 0O4;							// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (is_same <decltype (a), int>::value, "");
auto b = 0O777U;						// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (is_same <decltype (b), unsigned>::value, "");
auto c = 0O4l;							// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (is_same <decltype (c), long>::value, "");
auto d = 0O777UL;						// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (is_same <decltype (d), unsigned long>::value, "");
auto e = 0O4LL;							// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (is_same <decltype (e), long long>::value, "");
auto f = 0O123456701234567ull;					// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (is_same <decltype (f), unsigned long long>::value, "");
#if 0o107							// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
#endif
#if 0O107							// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
#endif
#if 0o107 != 0x47						// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
#error
#endif
#if 0O13507 != 5959						// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
#error
#endif
static_assert (0o1 == 0x1, "");					// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O2 == 0x2, "");					// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o3 == 0x3, "");					// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O4 == 0x4, "");					// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o7U == 0x7U, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O10 == 0x8, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o17 == 0xf, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O20L == 0x10L, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o37 == 0x1f, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O40 == 0x20, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o77 == 0x3f, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O100 == 0x40, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o177 == 0x7f, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O200 == 0x80, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o377 == 0xff, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O400u == 0x100u, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o777 == 0x1ff, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O1000 == 0x200, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o1777 == 0x3ff, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O2000 == 0x400, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o3777 == 0x7ff, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O4000U == 0x800U, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o7777u == 0xfffu, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O10000L == 0x1000L, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o17777l == 0x1fffl, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O20000UL == 0x2000UL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o37777ULL == 0x3fffULL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O40000L == 0x4000L, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o77777 == 0x7fff, "");				// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O100000U == 0x8000U, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o177777L == 0xffffL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O200000UL == 0x10000UL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o377777L == 0x1ffffL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O400000l == 0x20000l, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o777777UL == 0x3ffffUL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O1000000L == 0x40000L, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o1777777l == 0x7ffffl, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O2000000L == 0x80000L, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o3777777UL == 0xfffffUL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O4000000l == 0x100000l, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o7777777UL == 0x1fffffUL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O10000000L == 0x200000L, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o17777777l == 0x3fffffl, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O20000000ULL == 0x400000ULL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o37777777UL == 0x7fffffUL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O40000000L == 0x800000L, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
#if __cplusplus >= 201402L
static_assert (0o7'7'7'7'7'7'77l == 0xffffffl, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target { c++14 && c++26_down } } }
static_assert (0O10'00'00'00'0L == 0x1000000L, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target { c++14 && c++26_down } } }
static_assert (0o1'77'77'77'77UL == 0x1ffffffUL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target { c++14 && c++26_down } } }
static_assert (0O200'000'000L == 0x2000000L, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target { c++14 && c++26_down } } }
static_assert (0o3'777'7'7'7'7'7L == 0x3ffffffL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target { c++14 && c++26_down } } }
#endif
static_assert (0O400000000UL == 0x4000000UL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o777777777L == 0x7ffffffL, "");			// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O1000000000l == 0x8000000l, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o1777777777UL == 0xfffffffUL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O2000000000L == 0x10000000L, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o3777777777L == 0x1fffffffL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O4000000000UL == 0x20000000UL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o7777777777UL == 0x3fffffffUL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O10000000000l == 0x40000000l, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o17777777777L == 0x7fffffffL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O20000000000UL == 0x80000000UL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O25053250513UL == 0xa8ad514bUL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o37777777777ul == 0xfffffffful, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O40000000000LL == 0x100000000LL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o77777777777ll == 0x1ffffffffll, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O100000000000LL == 0x200000000LL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o177777777777ULL == 0x3ffffffffULL, "");	// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O200000000000ll == 0x400000000ll, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o777777777777777777777LL == 0x7fffffffffffffffLL, "");		// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O1000000000000000000000ULL == 0x8000000000000000ULL, "");	// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0O1011617105572074461270ULL == 0x8271e45bd0f262b8ULL, "");	// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
static_assert (0o1777777777777777777777ULL == 0xffffffffffffffffULL, "");	// { dg-warning "'0o' prefixed constants are a C\\\+\\\+29 feature or GCC extension" "" { target c++26_down } }
