// { dg-do compile { target c++11 } }

constexpr const char *f1 (const char *p, int q) { return __builtin_strchr (p, q); }
constexpr const char *f2 (const char *p, int q) { return __builtin_index (p, q); }
constexpr const char *f3 (const char *p, int q) { return __builtin_strrchr (p, q); }
constexpr const char *f4 (const char *p, int q) { return __builtin_rindex (p, q); }
constexpr const char a[] = "abcdefedcba";
static_assert (f1 ("abcde", 'f') == nullptr, "");
static_assert (f1 (a, 'g') == nullptr, "");
static_assert (f1 (a, 'f') == a + 5, "");
static_assert (f1 (a, 'c') == a + 2, "");
static_assert (f1 (a, '\0') == a + 11, "");
static_assert (f2 ("abcde", 'f') == nullptr, "");
static_assert (f2 (a, 'g') == nullptr, "");
static_assert (f2 (a, 'f') == a + 5, "");
static_assert (f2 (a, 'c') == a + 2, "");
static_assert (f2 (a, '\0') == a + 11, "");
static_assert (f3 ("abcde", 'f') == nullptr, "");
static_assert (f3 (a, 'g') == nullptr, "");
static_assert (f3 (a, 'f') == a + 5, "");
static_assert (f3 (a, 'c') == a + 8, "");
static_assert (f3 (a, '\0') == a + 11, "");
static_assert (f4 ("abcde", 'f') == nullptr, "");
static_assert (f4 (a, 'g') == nullptr, "");
static_assert (f4 (a, 'f') == a + 5, "");
static_assert (f4 (a, 'c') == a + 8, "");
static_assert (f4 (a, '\0') == a + 11, "");
