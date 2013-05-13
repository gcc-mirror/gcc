// PR c++/34949
// { dg-do compile }
// { dg-options "-O3" }

struct E {};
struct A
{
  virtual void a (void *) = 0;
};
struct B
{
  virtual ~B () {};
  unsigned int b1;
  E **b2;
  A *b3;
};
struct C : public B
{
  ~C ();
};
C::~C ()
{
  for (unsigned int i = 0; i < b1; i++)
    b3->a (b2);
}
struct D
{
  ~D () {}
  C d;
};
struct F { virtual ~F () {}; };
struct G { void g (); };
struct H : public F
{
  virtual ~H ();
  D *h1;
  G *h2;
};
H::~H ()
{
  h2->g ();
  delete h1;
}
