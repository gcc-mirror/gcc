// { dg-do assemble  }
// Bug: g++ doesn't push parameter decls as they are parsed.

void (*ptr) (int foo, int array[sizeof(foo)]); // { dg-bogus "" "" { xfail *-*-* } }  
void test2 (int bar, int array[sizeof(bar)]) { } // { dg-bogus "" "" { xfail *-*-* } }  
