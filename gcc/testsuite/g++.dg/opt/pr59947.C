// PR ipa/59947
// { dg-do compile { target c++11 } }
// { dg-options "-O0" }

#pragma GCC optimize ("O2")
template <typename T>
inline void
foo (T & a) noexcept { T tmp = static_cast <T &&> (a); };
struct A
{
  A () noexcept : a (1), b (1) {}
  virtual void c () noexcept = 0;
  void d () noexcept { c (); }
  int a;
  int b;
};
struct B
{
  ~B () noexcept { e->d (); }
  A *e;
};
template <typename T>
struct C
{
  B f;
};
struct D {};
template <typename T>
struct E
{
  void bar () { foo (g); }
  C <D> g;
};
template class E <char>;
