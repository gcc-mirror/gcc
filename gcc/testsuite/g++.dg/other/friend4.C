// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// Misleading diagnostic

struct A
{
  friend void A::foo();  // { dg-error "3:member functions are implicitly friends" }
  friend A::~A();        // { dg-error "3:member functions are implicitly friends" }
};
