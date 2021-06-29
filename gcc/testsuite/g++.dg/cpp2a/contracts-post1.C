// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

int f1(int n)
  [[pre: n >= 0]]
  [[post r: r >= 0]]
  [[post r: !(r < 0)]]
{
  return n;
}

int f2(int n)
  [[post: true]]
{
  return 0;
}

int f3(int n)
  [[post r: r >= n]]
{
  return n + 1;
}

int f4(int n)
  [[post: x > 0]] // { dg-error "not declared" }
{
  return 0;
}

void f5()
  [[post: true]]
{ }

void f6()
  [[post r: true]] // { dg-error "function does not return a value" }
{ }

int f7(int n)
  [[post: n > 0]]
{
  return x; // { dg-error "not declared" }
}

void f8(int n)
  [[post: n > 0]]
{
  return;
}

void f9(int n)
  [[post: n > 0]]
{
  return n; // { dg-error "return-statement with a value" }
}

int f10(int n)
  [[post: n > 0]]
{
  return; // { dg-error "return-statement with no value" }
}

void f11()
  [[post: true]]
{
  constexpr int n = 0;
  return n; // { dg-error "return-statement with a value" }
}

int f12()
  [[post: true]]
{
  return; // { dg-error "return-statement with no value" }
}

