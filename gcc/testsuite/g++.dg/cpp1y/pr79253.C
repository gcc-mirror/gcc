// { dg-do compile { target c++14 } }
// PR 79253 ICE instantiating lambda body.

template <typename> struct A;
template <typename = A<int>> class B {};
template <class T, class U, class V> void foo (U, V) { T (0, 0); }
struct C {};
template <template <bool, bool, bool> class F, class>
void
bar ()
{
  F<0, 0, 0>::baz;
}
struct G { int l; };
template <int, int, int> struct E : C
{
  E (int, int) : e (0)
  {
    auto &m = this->m ();
    auto c = [&] { m.l; };
  }
  G &m ();
  int e;
};
struct D
{
  void
  baz () { bar<F, B<>>; }
  template <bool, bool, bool> struct F
  {
    static B<> baz () { foo<E<0, 0, 0>> (0, 0); return B<>(); }
  };
};
