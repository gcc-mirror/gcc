// PR c++/83993
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern const int a[];
const int b[5] = { 1, 2, 3, 4, 5 };
extern const int c[4];
constexpr const int *d = &a[0];
constexpr const int *d2 = a;
constexpr const int *e = &a[1];		// { dg-error "nonzero array subscript '1' is used with array 'a' of type 'const int \\\[\\\]' with unknown bounds" }
constexpr const int *f = &b[0];
constexpr const int *f2 = b;
constexpr const int *g = &b[5];
constexpr const int *h = &b[6];		// { dg-error "array subscript value '6' is outside the bounds of array 'b' of type 'const int \\\[5\\\]'" }
constexpr const int *i = &c[0];
constexpr const int *i2 = c;
constexpr const int *j = &c[4];
constexpr const int *k = &c[5];		// { dg-error "array subscript value '5' is outside the bounds of array 'c' of type 'const int \\\[4\\\]'" }
extern const int l[];

void
foo ()
{
  extern const int l[3];
  constexpr const int *m = &l[0];
  constexpr const int *m2 = l;
  constexpr const int *n = &l[1];
  static_assert (m == m2, "");
}

constexpr const int *m = &l[0];
constexpr const int *m2 = l;
constexpr const int *n = &l[1];		// { dg-error "nonzero array subscript '1' is used with array 'l' of type 'const int \\\[\\\]' with unknown bounds" }
static_assert (d == d2 && f == f2 && i == i2 && m == m2, "");
const int o[] = { 1, 2 };
constexpr const int *p = &o[0];
constexpr const int *p2 = o;
constexpr const int *q = &o[2];
constexpr const int *r = &o[3];		// { dg-error "array subscript value '3' is outside the bounds of array 'o' of type 'const int \\\[2\\\]'" }
struct S { char a; char b[]; } s;
constexpr const char *t = &s.b[0];
constexpr const char *t2 = s.b;
constexpr const char *u = &s.b[1];	// { dg-error "nonzero array subscript '1' is used with array of type 'char \\\[\\\]' with unknown bounds" }
struct V { int a; };
extern V v[];
constexpr V *w = &v[0];
constexpr V *w2 = v;
constexpr int *x = &v[0].a;
constexpr int y = a[0];			// { dg-error "the value of 'a' is not usable in a constant expression" }
