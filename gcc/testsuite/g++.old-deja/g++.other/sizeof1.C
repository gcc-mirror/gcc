// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

struct X {
  int f ();
  int f (int);
};

void f(int i)
{
  i = sizeof(X::f); // { dg-error "" } cannot take sizeof a member function
}
