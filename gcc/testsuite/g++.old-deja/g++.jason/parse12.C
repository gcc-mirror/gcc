// PRMS Id: 6821

struct A {
  int operator()(int i) { return i; }
};

struct B {
  A* p;
  int f () { return (*p)(42); }	// gets bogus error
};

int main ()
{
  B b = { new A };

  return b.f () != 42;
}
