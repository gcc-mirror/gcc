// { dg-do compile }
// { dg-options "" }

#include "dtor2.h"

A::A () {}
A::~A () {}

void B::mb () {}
B::B (int) {}
B::~B () {}

void C::mc () {}
C::C (int x) : B (x) {}

D::~D () {}
