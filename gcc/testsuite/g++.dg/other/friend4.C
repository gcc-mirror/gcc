// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// Misleading diagnostic

struct A
{
  friend void A::foo();  // { dg-error "implicitly friends" }
  friend A::~A();        // { dg-error "implicitly friends" }
};
