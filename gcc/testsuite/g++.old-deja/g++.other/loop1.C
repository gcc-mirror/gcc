// Test for bad loop optimization of goto fixups.
// Special g++ Options: -O2

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

main ()
{
  f (test);
  return (c != d);
}
