// PR c++/64679

struct Bar {
  Bar (int, int, int);
};

void
g ()
{
  Bar e1(int(x), int(x), int); // { dg-error "redefinition" }
  Bar e2(int (*p)(int(x), int(x)), int); // { dg-error "redefinition" }
}
