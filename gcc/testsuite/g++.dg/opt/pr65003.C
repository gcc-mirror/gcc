// PR middle-end/65003
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-fpic" { target fpic } }

struct A
{
  void operator= (A &);
  A ();
};
struct B
{
  A b;
};
struct C
{
  virtual bool foo (int &, bool) const;
};
struct D : virtual C
{
  bool foo (int &, bool) const;
  B e;
};
struct F : D
{
  F (int &, const int &, const A &);
  bool foo (int &, bool) const;
};
bool D::foo (int &, bool) const { return true; }
F::F (int &, const int &, const A &) {}
bool F::foo (int &, bool) const { return false; }
