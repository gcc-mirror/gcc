// PR c++/13377
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

namespace N
{
  int i;            // { dg-error "declared" }
}

int i;              // { dg-error "declared" }

using namespace N;

void foo() { i; }   // { dg-error "in this scope|ambiguous" }
