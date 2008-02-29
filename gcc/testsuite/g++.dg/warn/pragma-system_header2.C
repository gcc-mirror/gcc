// PR c++/30500
// { dg-options "-Wconversion" }

#include "pragma-system_header2.h"


void f()
{
  g<int>();
  h<int>();
}

// { dg-warning "conversion" "" { target *-*-* } 2 }
// { dg-warning "conversion" "" { target *-*-* } 5 }

// I couldn't find another way to make this work.
// { dg-prune-output "In file included from" }
