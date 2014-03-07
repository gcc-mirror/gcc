// PR c++/45894
// { dg-do compile { target c++11 } }
// { dg-options "-Wsequence-point" }

struct F
{
  template <typename = int>
  void bar ();
};
template <typename = int>
struct V
{
  V (const V &) { F::bar <>; }
};
struct C
{
  V <> v;
};
struct B
{
  C f ();
};
struct A
{
  C c;
  B b;
  A () : c (b.f ()) { }
};
