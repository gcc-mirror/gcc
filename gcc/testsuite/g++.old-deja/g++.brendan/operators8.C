// { dg-do assemble  }
// GROUPS passed operators
struct A {
  char *p;
  operator char *();
};

char foo(A a)
{
  return a[0];
}
