// Build don't link: 
// GROUPS passed nested-classes
class A {
  int x;

  struct B {
    int x;
  };
  struct C;
  friend struct C;
  struct C {
    int bug (A::B &y);
  };
};

int
A::C::bug (A::B &y)
{
  return y.x;
}

