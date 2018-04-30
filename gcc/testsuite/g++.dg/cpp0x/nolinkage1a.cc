#include "nolinkage1.h"

typedef struct { double d; } *BP;

void f(BP) {  }

A<BP> b;

static void g()
{
  struct B { };
  A<B> a;
}

int dummy() { g(); f(0); return 0; }
