#include "dtor1.h"

extern "C" void abort ();

int d = 5;

B::B (int i) : x (i) { }
B::~B () { if (d-- != x) abort (); }

C1::C1 (int i) : B (i) {}

C2::C2 (int i) : B (i) {}

D::D (int i) : B (i) {}

E::E (int i) : B (i) {}

A::A () : D (0), E (1), C1 (2), C2 (3), x1(4), x2(5) {}
