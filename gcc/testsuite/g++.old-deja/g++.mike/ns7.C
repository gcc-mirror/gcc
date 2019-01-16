// { dg-do assemble  }

namespace A {
  int i = 1;			// { dg-message "A::i" }
}

namespace B {
  int j = i;	// { dg-error "'i' was not declared in this scope; did you mean 'A::i'" } 
}
