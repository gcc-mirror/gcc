// { dg-do assemble  }
#include <typeinfo>

class A {
public:
  virtual void j () {}
};

class B : public A { };
     
void x (A& a) {
  const B& b1 = dynamic_cast<B&>((const A&)a);	// { dg-error "" } opps
}
