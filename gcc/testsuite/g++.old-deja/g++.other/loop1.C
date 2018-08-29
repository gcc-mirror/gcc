// { dg-do run  }
// { dg-options "-O2" }
// Test for bad loop optimization of goto fixups.

typedef bool (*ftype) ();

int c, d;
struct A {
  A() { ++c; }
  A(const A&) { ++c; }
  ~A() { ++d; }
};

void f (ftype func)
{
  A a;
  do {
    if ((*func)()) return;
  } while (true);
}

bool test ()
{
  return true;
}

int
main ()
{
  f (test);
  return (c != d);
}
