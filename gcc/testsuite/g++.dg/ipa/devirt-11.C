/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-cp -fdump-ipa-inline -fno-devirtualize-speculatively" } */
int baz ();
struct A
{
  virtual int fn2 () = 0;
  virtual int *fn3 ();
  double *fn4 ();
  int fn5 (int);
  template <class T>
  void fn1 (A &, T) { fn3 (); fn4 (); fn2 (); }
};
struct B : A
{
  int fn2 () { return 6; }
  void fn3 (int, double);
  B (bool = true);
  B (int, int);
};
template <typename T>
void
foo (B &x, A &y, A &z)
{
  y.fn2 ();
  z.fn2 ();
  int i = baz ();
  int j = (y.fn3 ())[i];
  x.fn3 (j, (y.fn4 ())[i] + (z.fn4 ())[z.fn5 (j)]);
}
inline B
operator+ (A &y, A &z)
{
  B x;
  foo<int> (x, y, z);
  return x;
}
void
bar ()
{
  B a, b, c (4, 0), d;
  a.fn1 (b, .6);
  baz ();
  c + d;
}
/* { dg-final { scan-ipa-dump "Discovered a virtual call to a known target" "inline"  } } */
