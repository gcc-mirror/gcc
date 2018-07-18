// { dg-do compile { target c++11 } }

constexpr const char *f1 (const char *p, const char *q) { return __builtin_strstr (p, q); }
constexpr const char a[] = "abcdefedcbaaaaab";
constexpr const char b[] = "fed";
constexpr const char c[] = "aaab";
static_assert (f1 ("abcde", "ee") == nullptr, "");
static_assert (f1 (a, b) == a + 5, "");
static_assert (f1 (a, c) == a + 12, "");
static_assert (f1 (a, "") == a, "");
static_assert (f1 (a, "aaaaaab") == nullptr, "");
static_assert (f1 (a, "aaa") == a + 10, "");
