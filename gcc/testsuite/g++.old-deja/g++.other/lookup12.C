// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct B {
  struct X {};
};


struct D : public B {
  void X();

  struct X x;

  void f();
};


void D::f() 
{
  struct X y;
}
