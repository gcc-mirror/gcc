// { dg-do assemble  }
// Test for proper diagnostics on trying to take the address of a non-static
// member function.

struct A {
  void f ();
  void f (int);
  void g ();
};

int main ()
{
  A a;
  &a.f;				// { dg-error "" } overloaded
  &a.g;				// { dg-error "" } can't write a pmf like this
}
