// Testcase for the lifetime of a temporary object which is used to
// initialize a reference.

int destroyed = 0;

struct A {
  A() { }
  A(int) { }
  ~A() { destroyed++; }
};

A a;
A foo () { return a; }

int main()
{
  const A& ar = foo();
  const A& ar2 = A();
  const A& ar3 = (A)1;
  return destroyed;
}
