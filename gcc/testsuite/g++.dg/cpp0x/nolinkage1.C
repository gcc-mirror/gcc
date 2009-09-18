// DR 757 allows using types without linkage in declarations with linkage.
// Test that this doesn't lead to link-time collisions.

// { dg-additional-sources "nolinkage1a.cc" }
// { dg-do link }

#include "nolinkage1.h"

typedef struct { int i; } *AP;

void f(AP) { }

A<AP> a;

static void g()
{
  struct B { };
  A<B> a;
}

int main() { g(); f(0); }
