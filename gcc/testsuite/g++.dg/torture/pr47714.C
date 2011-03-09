struct A { virtual ~A () {} };
struct B { virtual ~B () {} };
struct C { virtual const A *foo (int) const = 0; };
struct E : public B, public A { };
struct F : public C
{
  virtual const E *foo (int) const;
};
void bar (int &);

const E *
F::foo (int x) const
{
  bar (x);
  return __null;
}
