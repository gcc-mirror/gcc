// PR c++/84463
// { dg-do compile { target c++11 } }

struct S { int r; const unsigned char s[5]; };
static constexpr S a[] = { { 0, "abcd" } };
struct T { const unsigned char s[5]; };
static constexpr T b[] = { { "abcd" } };

constexpr int
foo (const unsigned char *x)
{
  return x[0];
}

constexpr static const S *j = &a[0];
constexpr static const int k = j->s[0];
constexpr static int l = foo (a[0].s);
constexpr static int m = foo (j->s);
constexpr static const T *n = &b[0];
constexpr static const int o = n->s[0];
constexpr static int p = foo (b[0].s);
constexpr static int q = foo (n->s);
