// PR c++/13377
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

namespace N
{
  int i;            // { dg-message "i" }
}

int i;              // { dg-message "i" }

using namespace N;

void foo() { i; }   // { dg-error "ambiguous" }
