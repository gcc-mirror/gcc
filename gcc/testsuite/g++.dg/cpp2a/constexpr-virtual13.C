// PR c++/88110
// { dg-do compile }

struct A {
  virtual int foo () const = 0;
};
struct B {
  virtual int bar () const = 0;
  virtual int baz () const = 0;
};
struct C : public A { };
struct D : public C { };
struct E : public D, public B { };

void
qux (const E *x)
{
  if (x->baz ())
    ;
}
