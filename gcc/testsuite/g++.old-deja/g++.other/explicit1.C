// { dg-do assemble  }
// Origin: Jason Merrill <jason@redhat.com>

struct A
{
  A ();
  explicit A (int);
};

int main ()
{
  const A& r = 1;		// { dg-error "" } no suitable constructor
}
