//Build don't link:
class A {
private:
  int i1_;
public:
  void f(int const i1 = 1); // ERROR - previous specification
};

void
A::f(int const i1 = 1)
{                          // ERROR - duplicate default argument
  i1_ = i1;
}

int
main()
{
  A a;
  a.f();
  return 0;
}
