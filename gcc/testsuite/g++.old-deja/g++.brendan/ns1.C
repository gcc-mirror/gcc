// { dg-do assemble  }
// GROUPS passed old-abort
struct B 
{
      void f(char); 
      void g(char);
};
  
class C 
{
  int g();
};

class D2 : public B 
{
  using B::f;  // ok: B is a base of D2
  using C::g;  // { dg-error "not a base type" }
};
