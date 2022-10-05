// The static_cast should prevent temporary elision.
// { dg-do run { target c++11 } }

int d;
struct A
{
  int i;
  A() { }
  ~A() { ++d; }
};

A f() { return A(); }

struct B
{
  A a;
  B(): a(static_cast<A&&>(f())) {}
};

int main()
{
  { B b; }
  if (d != 2)
    return -1;
}
