// { dg-do assemble  }
class A {
private:
  int i1_;
public:
  void f(int const i1 = 1); // { dg-error "" } previous specification
};

void
A::f(int const i1 = 1)
{                          // { dg-error "" } duplicate default argument
  i1_ = i1;
}

int
main()
{
  A a;
  a.f();
  return 0;
}
