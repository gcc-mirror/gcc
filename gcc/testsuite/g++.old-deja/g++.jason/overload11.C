// PRMS Id: 4697
// Bug: g++ calls the non-const method for a const object.

class A {
public:
  void foo(int &i) const { i = 0; }
  void foo(int &i) { i = 1; }
};

int main()
{
  A a;
  const A& b = a;
  int i = 2;
  b.foo (i);
  return i;
}
