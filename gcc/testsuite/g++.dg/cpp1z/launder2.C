// { dg-do compile { target c++11 } }

int a;
int *b = __builtin_launder ();		// { dg-error "wrong number of arguments to" }
int *c = __builtin_launder (&a, 2);	// { dg-error "wrong number of arguments to" }
int *d = __builtin_launder (&a);
int e = __builtin_launder (a);		// { dg-error "not a pointer to object type" }
int &f = a;
int g = __builtin_launder (f);		// { dg-error "not a pointer to object type" }

template <typename T> T f1 (T x) { return __builtin_launder (x); }	// { dg-error "not a pointer to object type" }
template <typename T> T f2 (T x) { return __builtin_launder (x); }

int h = f1 (a);
int *i = f2 (&a);
struct S { long s; int foo (); } *j;
S *k = f2 (j);
int l = __builtin_launder (j)->foo ();

template <typename T> T *f3 (T *x) { return __builtin_launder (x); }

long *m;
long *n = f3 (m);
int *o = f3 (&a);

template <typename T, typename... U> T *f4 (U... x) { return __builtin_launder (x...); }
template <typename T, typename... U> T *f5 (U... x) { return __builtin_launder (x...); }	// { dg-error "wrong number of arguments to" }
template <typename T, typename... U> T *f6 (U... x) { return __builtin_launder (x...); }	// { dg-error "wrong number of arguments to" }
template <typename T, typename... U> T f7 (T x, U... y) { return __builtin_launder (x, y...); }	// { dg-error "wrong number of arguments to" }

long *p = f4<long, long *> (m);
long *q = f5<long> ();
long *r = f6<long, long *, int> (m, 1);
S s;
int t = __builtin_launder (&s)->foo ();

constexpr const int *f8 (const int *x) { return __builtin_launder (x); }
template <typename T> constexpr T f9 (T x) { return __builtin_launder (x); }
constexpr int u = 6;
constexpr const int *v = f8 (&u);
constexpr const int *w = f9 (&u);
static_assert (*f8 (&u) == 6 && *f9 (&u) == 6, "");
