// Build don't link:

namespace A {	// WARNING - namespaces mostly broken
  int i = 1;
}

namespace B {
  int j = i;	// ERROR - XFAIL *-*-*
}
