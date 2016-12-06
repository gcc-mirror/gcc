// { dg-do compile { target c++11 } }

typedef decltype (sizeof (0)) size_t;
constexpr const void *f1 (const char *p, int q) { return __builtin_memchr (p, q, __builtin_strlen (p) + 1); }
constexpr const void *f2 (const char *p, int q, size_t r) { return __builtin_memchr (p, q, r); }
constexpr const char a[] = "abcdefedcba";
static_assert (f1 ("abcde", 'f') == nullptr, "");
static_assert (f1 (a, 'g') == nullptr, "");
static_assert (f1 (a, 'f') == a + 5, "");
static_assert (f1 (a, 'c') == a + 2, "");
static_assert (f1 (a, '\0') == a + 11, "");
static_assert (f2 ("abcde", 'f', 6) == nullptr, "");
static_assert (f2 ("abcde", 'f', 1) == nullptr, "");
static_assert (f2 ("abcde", 'f', 0) == nullptr, "");
static_assert (f2 (a, 'g', 7) == nullptr, "");
static_assert (f2 (a, 'g', 0) == nullptr, "");
static_assert (f2 (a, 'f', 6) == a + 5, "");
static_assert (f2 (a, 'f', 5) == nullptr, "");
static_assert (f2 (a, 'c', 12) == a + 2, "");
static_assert (f2 (a, 'c', 3) == a + 2, "");
static_assert (f2 (a, 'c', 2) == nullptr, "");
static_assert (f2 (a, '\0', 12) == a + 11, "");
static_assert (f2 (a, '\0', 11) == nullptr, "");
static_assert (f2 (a, '\0', 0) == nullptr, "");
