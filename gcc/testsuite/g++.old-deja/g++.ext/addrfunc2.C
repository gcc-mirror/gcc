// Test for implicit & on methods.
// Contributed by Jason Merrill <jason@cygnus.com>.
// Special g++ Options: -fpermissive -w

struct A {
  void f (int = 0) { }
};

int
main ()
{
  void (A::*p)(int) = 0;
  p = A::f;
  if (p != A::f)
    return 1;
}
