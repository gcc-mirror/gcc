#include "elide1.h"

int d;

A::A () { }
A::A (const A&) { }
A::~A() { ++d; }

void f (A a) { }
