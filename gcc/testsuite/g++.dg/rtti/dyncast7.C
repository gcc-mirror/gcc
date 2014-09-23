// I think this dynamic_cast has undefined behavior when destroying E::o
// because we're the F period of destruction has started and ap doesn't
// point to the object currently being destroyed--but the reasonable
// options are success or failure, not SEGV.

// { dg-do run }

extern "C" void abort();

struct A { virtual ~A(); };
struct B { virtual ~B() { } };
struct C : B, A { };
struct E : virtual B { A o; };
struct F : virtual C, virtual E { };

A* ap;
C* cp;

A::~A() {
  C* cp2 = dynamic_cast<C*>(ap);
  if (cp2 != cp && cp2 != 0)
    abort();
}

int main() {
  F f;
  ap = cp = &f;
}
