// { dg-do assemble  }

namespace A {
  int i = 1;			// { dg-message "A::i" }
}

namespace B {
  int j = i;	// { dg-error "" } 
  // { dg-message "suggested alternative" "suggested alternative" { target *-*-* } 8 }
}
