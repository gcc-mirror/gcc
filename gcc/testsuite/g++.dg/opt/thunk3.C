// PR c++/39106
// { dg-do compile }
// { dg-options "-O2" }

extern "C" void abort ();

struct A
{
  A (bool x = true);
};
class B
{
  virtual bool bar (A &, int) const =0;
};
class C : virtual public B
{
};
struct D : virtual public B
{
  bool bar (A &, int) const;
};
template <int N>
struct E : public D
{
  bool bar (A &x, int y) const
  {
    return baz().bar (x, y);
  }
  const D & baz () const;
};
extern template class E<0>;

void
foo ()
{
  try
  {
    A a;
    abort ();
  } catch (...)
  {
  }
  A b;
  E<0> c;
  c.bar (b, 3);
  E<0> d;
  d.bar (b, 3);
}
