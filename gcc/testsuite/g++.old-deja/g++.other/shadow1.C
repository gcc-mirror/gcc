// Bug: g++ thinks that the i in g() shadows the parm from f()
// Contributed by Jason Merrill <jason@cygnus.com>
// Build don't link:

void f (int i)
{
  struct A {
    void g () {
      int i;
    }
  };
}
