// Build don't link:

namespace A {
  int i = 1;
}

namespace B {
  int j = i;	// ERROR - 
}
