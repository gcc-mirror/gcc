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
  &a.f;				// ERROR - overloaded
  &a.g;				// ERROR - can't write a pmf like this
}
