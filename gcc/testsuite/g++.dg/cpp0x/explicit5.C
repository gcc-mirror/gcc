// test for extension of DR 899 to handle template ctors
// { dg-options "-std=c++11" }
// { dg-do run }

int r = 1;

struct C {
  C() { }
  template <class T = int> C(C&, T = 0) { r = 0; }
};

C c;

struct A
{
  explicit operator C&() const { return c; }
};

int main()
{
  A a;
  C c2 (a);

  return r;
}
