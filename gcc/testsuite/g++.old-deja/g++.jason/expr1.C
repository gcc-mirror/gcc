// Bug: g++ doesn't figure out what to do.
// Build don't link:

struct A {
  operator char *();
};
 
char foo(A a)
{
  char c = a[0];			// gets bogus error
  return c;
}
