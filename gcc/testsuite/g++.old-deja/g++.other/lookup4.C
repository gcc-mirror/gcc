// { dg-do run  }
// Test for proper handling of references to overloaded member functions.

struct A {
  static void f (int) { }
  void f ();
};

void (*p)(int) = &A::f;

void A::f ()
{
  p = f;
}

int main()
{
  A a;
  p = &a.f;
  (a.f)();
  (a.f)(42);
}
