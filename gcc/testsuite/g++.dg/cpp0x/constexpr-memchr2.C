// PR c++/127218
// { dg-do compile { target c++11 } }
// The pointer-returning string builtins must not count a nonzero offset in
// their first argument twice when the front end folds them.

constexpr const char a[] = "abcdefghijabxdefghijaaa";
static_assert (__builtin_memchr (a, 'x', sizeof (a) - 1) == a + 12, "");
static_assert (__builtin_memchr (a + 1, 'a', sizeof (a) - 2) == a + 10, "");
static_assert (__builtin_memchr (a + 2, 'a', sizeof (a) - 3) == a + 10, "");
static_assert (__builtin_memchr (a + 1, 'x', sizeof (a) - 2) == a + 12, "");
static_assert (__builtin_memchr (a + 1, 'q', sizeof (a) - 2) == nullptr, "");
static_assert (__builtin_memchr (&a[1], 'a', sizeof (a) - 2) == a + 10, "");
static_assert (__builtin_strchr (a + 1, 'a') == a + 10, "");
static_assert (__builtin_strchr (a + 1, '\0') == a + 23, "");
static_assert (__builtin_strrchr (a + 1, 'b') == a + 11, "");
static_assert (__builtin_strrchr (a + 3, 'a') == a + 22, "");
static_assert (__builtin_strstr (a + 1, "ab") == a + 10, "");
static_assert (__builtin_strstr (a + 1, "") == a + 1, "");
static_assert (__builtin_strstr (a + 1, "zz") == nullptr, "");
static_assert (__builtin_strlen (a + 1) == 22, "");

constexpr const void *f1 (const char *p, int q, unsigned long n)
{ return __builtin_memchr (p, q, n); }
constexpr const char *f2 (const char *p, int q) { return __builtin_strchr (p, q); }
constexpr const char *f3 (const char *p, int q) { return __builtin_strrchr (p, q); }
constexpr const char *f4 (const char *p, const char *q) { return __builtin_strstr (p, q); }
static_assert (f1 (a + 1, 'a', sizeof (a) - 2) == a + 10, "");
static_assert (f2 (a + 1, 'a') == a + 10, "");
static_assert (f3 (a + 1, 'b') == a + 11, "");
static_assert (f4 (a + 1, "ab") == a + 10, "");
