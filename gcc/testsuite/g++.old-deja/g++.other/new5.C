// { dg-do run  }
// Test that const-correctness is observed when using new.

struct A {
  A() { }
  int f () { return 1; }
  int f () const { return 0; }
};

int main ()
{
  return (new const A)->f ();
}
