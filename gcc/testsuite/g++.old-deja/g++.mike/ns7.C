// { dg-do assemble  }

namespace A {
  int i = 1;
}

namespace B {
  int j = i;	// { dg-error "" } 
}
