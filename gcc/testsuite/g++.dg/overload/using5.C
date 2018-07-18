// Core issues 2273, 2277
// { dg-do compile { target c++11 } }

struct A
{
  A(int, int = 0);
  static void f(int = 0);
};

struct B: A
{
  using A::A;
  B(int);

  using A::f;
  static void f();
};

struct C: B {
  using B::B;
  using B::f;
};
  
int main()
{
  C c (42);
  c.f();
}
