// { dg-do assemble  }
struct A { int i; };

A a = {{{1}}};			// { dg-error "" } causes abort
