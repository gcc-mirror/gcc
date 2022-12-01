// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct S
{
  S() [[post: n == 0]]
    : n(0)
  { }

  ~S() [[post: true]]
  { }

  int f1()
    [[post r: n == r]]
  {
    return n;
  }

  int f2()
    [[post r: r == x]] // { dg-error "not declared" }
  {
    return n;
  }

  void f3()
    [[post r: n]] // { dg-error "does not return a value" }
  {
  }

  int n = 0;
};

int main()
{
  // f1(0);
}
