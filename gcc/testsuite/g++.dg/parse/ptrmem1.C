struct A {
  void f(int = 0) const;
};

typedef void (A::*PF)(int) const;

void f()
{
  PF pf = &A::f;
}
