// Test that taking the address of a member function name produces
// a pointer to member function.
// Contributed by Jason Merrill <jason@cygnus.com>
// Special g++ Options: -fpermissive -w
// Build don't link:

struct A { };
int (A::*p)();

struct B {
  int f () { return 0; }
  void g ();
};

void B::g ()
{
  p = (int (A::*)())&f;
}
