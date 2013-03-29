// { dg-options -std=c++1y }
// { dg-do run }

int c;
int d;

struct A
{
  A() { ++c; }
  A(const A&) { ++c; }
  ~A() { ++d; }
};

A g() { return A(); }
decltype(auto) f() { return g(); }

int main()
{
  f();
  if (c < 1 || c != d)
    __builtin_abort ();
}
