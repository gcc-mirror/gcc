// PR c++/58207
// { dg-do compile { target c++11 } }

struct A
{
  virtual bool foo ();
};

struct B : public A
{
  constexpr B () : A (&::n) {}  // { dg-error "declared" }
};
