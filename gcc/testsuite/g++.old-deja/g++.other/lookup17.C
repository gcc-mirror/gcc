// Bug: t->B is resolved to the type instead of the field.

struct A {
  struct B { } *B;
  int i, j, k, l, m;
};

struct A a;

int
main ()
{
  void *p = a.B;
}
