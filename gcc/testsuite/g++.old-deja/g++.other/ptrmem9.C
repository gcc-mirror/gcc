// Test that const-correctness is observed when using pointers-to-members.

struct A {
  int f () { return 1; }
  int f () const { return 0; }
};

struct B {
  A a;
  B() { }
};

int main ()
{
  A B::*bm = &B::a;
  const B b;
  return (b.*bm).f ();
}
