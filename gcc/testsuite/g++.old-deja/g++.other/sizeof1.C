// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

struct X {
  int f ();
  int f (int);
};

void f(int i)
{
  i = sizeof(X::f); // ERROR - cannot take sizeof a member function
}
