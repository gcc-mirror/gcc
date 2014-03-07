// PR c++/55542
// { dg-do compile { target c++11 } }

template <typename ... P>
struct B
{
  template <typename O>
  B (O *o, void (O::*f) (P ... p)) {}
};
class C
{
  void foo (void *, int);
  template <typename ... A>
  void bar (A ... a);
  B <void *> c;
  B <void *, int> d;
  C (int) : c (this, &C::bar), d (this, &C::foo) {}
};
template <typename ... A>
void C::bar (A ...)
{
}
