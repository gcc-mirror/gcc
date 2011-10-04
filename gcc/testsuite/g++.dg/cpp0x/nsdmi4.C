// { dg-do run }
// { dg-options -std=c++0x }

int c;

struct A
{
  A() { }
  A(const A&) { }
};

A f() { ++c; return A(); }

struct B
{
  A a = f();
};

int main()
{
  B b1, b2;
  if (c != 2)
    __builtin_abort();
}
