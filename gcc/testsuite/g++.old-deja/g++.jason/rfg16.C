// Bug: g++ doesn't push parameter decls as they are parsed.
// Build don't link:

void (*ptr) (int foo, int array[sizeof(foo)]); // gets bogus error XFAIL *-*-*
void test2 (int bar, int array[sizeof(bar)]) { } // gets bogus error XFAIL *-*-*
