// { dg-do assemble  }
// Bug: g++ doesn't figure out what to do.

struct A {
  operator char *();
};
 
char foo(A a)
{
  char c = a[0];			// { dg-bogus "" } 
  return c;
}
