// Compiles.   Shouldn't.
class A {
private:
  int i1_;
public:
  void f(int const i1 = 1);
};

void
A::f(int const i1 = 1) // !!! SHOULD TRIGGER AN ERROR !!!
{
  i1_ = i1;
}

int
main()
{
  A a;
  a.f();
  return 0;
}

