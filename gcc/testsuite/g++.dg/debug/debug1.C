// Test whether dwarf2 debug info works with named return value optimization
// { dg-do compile }

struct S
{
  virtual ~S();
  S (const char *str);
  S& operator= (const char *str);
  operator const char *() const;
  S& operator+= (const char *str);
};
inline S operator+ (const char *s1, const S &s2)
{
  S x (s1);
  x += s2;
  return x;
}
struct U
{
  U ();
  U& operator= (const char *);
  const char *foo() const;
  operator const char *() const { return foo(); }
};
template <class T> struct V
{
  T v;
};
template <class T> struct W
{
  V<T> *w;
  W() : w (0) {}
  const T& operator* () const { return w->v; }
  T& operator* () { return w->v; }
};
struct X {
  X();
};
struct Y {
  Y (const U &u);
};
X::X()
{
  W<U> a;
  U b;
  b = (*a) + "xx";
  Y c (b);
}
