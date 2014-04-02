// PR tree-optimization/57661
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-tree-forwprop" }

template <typename>
struct A
{
  ~A () {}
};
template <typename _Tp>
using B = A <_Tp>;
template <typename _Tp>
class C : B <_Tp> {};
namespace N { enum D { d }; }
template <class>
struct E
{
  ~E ();
};
template <class, class V>
struct F : V {};
template <class U, class V>
struct G : F <U, V>
{
  N::D g1;
  void g2 ();
  void g3 ();
  void g4 () { g3 (); }
  static void g5 (G *__t) { __t->g4 (); }
};
template <class U, class V>
struct H : G <U, V>
{
  E <U> *h1;
  bool h2;
  ~H () throw ()
  {
    this->g2 ();
    if (h2)
      delete h1;
  }
};
template <class U, class V>
struct I : H <U, V>, E <U>
{
  G <U, V> *i;
  ~I () throw ()
  {
    i->g4 ();
  }
};
struct J
{
  typedef C <char> j1;
  typedef G <char, C <char>> j2;
  J ();
  j2 *j3;
};
struct K : J
{
  typedef G <char, C <char>> j2;
  K () { j2::g5 (this->j3); }
};
template <class U, class V>
void G <U, V>::g3 ()
{
  switch (g1)
    {
    case N::d:
      {
	I <U, V> *q = (I <U, V> *) this;
	q->I <U, V>::~I ();
      }
    }
}
K r;
