// { dg-do run }
// PR c++/11750

struct A
{
  virtual void f() const { __builtin_abort(); }
  virtual void g() {}
};

struct B : virtual A
{
  virtual void f() const {}
  virtual void g() { __builtin_abort(); }
};

struct C : B, virtual A
{
  using A::f;
  using A::g;
};

int main()
{
  C c;
  c.f(); // call B::f

  C c2;
  c2.C::g(); // call A::g

  C* c3 = &c;
  c3->f(); // call B::f

  C& c4 = c;
  c4.f(); // call B::f

  C const* c5 = &c;
  c5->f(); // call B::f

  C** c6 = &c3;
  (*c6)->f(); // call B::f

  C const& c7 = c;
  c7.f(); // call B::f
}
