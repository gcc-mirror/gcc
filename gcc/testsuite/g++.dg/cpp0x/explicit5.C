// test for extension of DR 899 to handle template ctors
// { dg-do run { target c++11 } }

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
