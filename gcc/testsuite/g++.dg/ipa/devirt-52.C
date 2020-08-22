// PR middle-end/77259
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Wno-return-type" }

template <typename, typename = int> class A;
template <typename, typename> struct A
{
  A (A &&);
};
template <typename S, typename T, typename U>
A<S> operator+(S *, const A<T, U> &);
template <typename S, typename T, typename U>
void operator+(const A<T, U> &, S *);
struct B
{
  template <typename V> B (V);
};
template <typename V> V foo (B) {}
class C;
template <typename> struct D
{
  C *operator->() { return d; }
  C *d;
};
struct C
{
  virtual A<int> bar ();
};
struct E
{
  ~E ();
  virtual A<char> bar (const B &) const;
};
template <typename> struct F : E
{
};
template <typename W> struct F<D<W>> : E
{
  A<char> bar (const B &) const try
    {
      D<W> a = baz ();
    }
  catch (int)
    {
    }
  D<W> baz () const
  {
    D<C> b = foo<D<C>>(0);
    "" + b->bar () + "";
  }
};
struct G : F<D<int>>
{
  G (int);
};
void test () { G (0); }
