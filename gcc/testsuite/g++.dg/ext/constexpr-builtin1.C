// PR c++/80265
// { dg-do compile { target c++14 } }

constexpr bool compare() {
  char s1[] = "foo";
  char s2[] = "fxo";
  if (!__builtin_memcmp(s1, s2, 3))
    return false;
  s2[1] = 'o';
  if (__builtin_memcmp(s1, s2, 3))
    return false;
  if (__builtin_strcmp(s1, s2))
    return false;
  return true;
}

constexpr bool length() {
  char s[] = "foo";
  if (__builtin_strlen(s) != 3)
    return false;
  return true;
}

constexpr bool find() {
  char s[] = "foo";
  if (__builtin_memchr(s, 'f', 3) != s)
    return false;
  if (__builtin_strchr(s, 'o') != s+1)
    return false;
  if (__builtin_strstr(s, "oo") != s+1)
    return false;
  return true;
}

static_assert( compare(), "" );
static_assert( length(), "" );
static_assert( find(), "" );
