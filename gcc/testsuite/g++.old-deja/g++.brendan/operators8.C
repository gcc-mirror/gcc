// Build don't link: 
// GROUPS passed operators
struct A {
  char *p;
  operator char *();
};

char foo(A a)
{
  return a[0];
}
