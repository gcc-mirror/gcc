// Build don't link: 
// GROUPS passed old-abort
struct B 
{
      void f(char); 
      void g(char);
};
  
class C 
{ // ERROR -  warning
  int g();
};

class D2 : public B 
{ // ERROR - type C is not a base type for type D2
  using B::f;  // ok: B is a base of D
  using C::g;  // error: C isn't a base of D2
};
