// Build don't link:

class A;
struct XX { int A::*py; };

class A {
public:
  int p;
  void setp(XX *xp);
};

void A::setp(XX *xp) { xp->py = &A::p; }
