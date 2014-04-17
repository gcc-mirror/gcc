// { dg-do assemble  }
class A {
private:
  int i1_;
public:
  void f(int const i1 = 1); // { dg-message "previous specification" }
};

void
A::f(int const i1 = 1) // { dg-error "default argument given" }
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
