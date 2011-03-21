// { dg-do compile }
// { dg-options "-O3 -g" }

template <typename T>
T &max (T &a, T &b)
{
  if (a < b) return b; else return a;
}
int foo (double);
struct S
{
  struct T
  {
    int dims, count;
    T (int, int) : dims (), count () {}
  };
  T *rep;
  S () {}
  S (int r, int c) : rep (new T (r, c)) {}
  ~S () { delete rep; }
};
template <typename T>
struct U
{
  static T epsilon () throw ();
};
template <class T>
struct V
{
  struct W
  {
    T * data;
    int count;
    W (int n) : data (new T[n]), count () {}
  };
  V::W *rep;
  S dimensions;
  int slice_len;
  V (S s) : rep (new V <T>::W (get_size (s))) {}
  int capacity () { return slice_len; }
  int get_size (S);
};
template <class T>
struct Z : public V <T>
{
  Z () : V <T> (S (0, 0)) {}
  Z (int r, int c) : V <T> (S (r, c)) {}
};
template <class T>
struct A : public Z <T>
{
  A () : Z <T> () {}
  A (int n, int m) : Z <T> (n, m) {}
};
template <class T>
struct B : public V <T>
{
};
struct C : public A <double>
{
  C () : A <double> () {}
  C (int r, int c) : A <double> (r, c) {}
};
struct D : public B <double>
{
};
template <class T>
struct E
{
};
template <class T>
struct G : public E <T>
{
};
struct H : public G <double>
{
};
template <class R>
struct I
{
  R scl, sum;
  void accum (R val)
  {
    R t = __builtin_fabs (val);
    if (scl == t)
      sum += 1;
  }
  operator R () { __builtin_sqrt (sum); return R (); }
};
template <class R>
struct J
{
  template < class U > void accum (U val) {}
  operator R () { return R (); }
};
template <class R>
struct K
{
  R max;
  template <class U> void accum (U val)
  {
    double z = __builtin_fabs (val);
    max = ::max (max, z);
  }
  operator R () { return max; }
};
template <class R>
struct L
{
  unsigned num;
  template <class U> void accum (U) {}
  operator R () { return num; }
};
template <class T, class R, class S>
void bar (V <T> &v, R &res, S acc)
{
  for (int i = 0; i < v.capacity (); i++)
    acc.accum ((i));
  res = acc;
}
template <class T, class R>
void bar (B <T> &v, R)
{
  R res;
  bar (v, res, I <R> ());
}
template <class T, class R>
R bar (A <T> &v, R p)
{
  R res;
  if (p == 2)
    bar (v, res, I <R> ());
  else if (p == 1)
    bar (v, res, J <R> ());
  else if (p == sizeof (float) ? (p) : foo (p))
    {
      if (p > 0)
	bar (v, res, K <R> ());
    }
  else if (p == 0)
    bar (v, res, L <R> ());
  return res;
}
template <class CT, class VectorT, class R>
void
baz (CT m, R p, R tol, int maxiter, VectorT)
{
  VectorT y (0, 0), z (0, 1);
  R q = 0;
  R gamma = 0, gamma1 = 0;
  gamma = bar (y, p);
  (void) (bar (z, q) <= (gamma1 <= gamma));
}
int a = 100;
template <class CT, class VectorT, class R>
void
test (CT m, R p, VectorT)
{
  VectorT x;
  R sqrteps (U <R>::epsilon ());
  baz (m, p, sqrteps, a, x);
}
void
fn (D x, double p)
{
  bar (x, p);
}
void
fn (H x, double p)
{
  test (x, p, C ());
}
