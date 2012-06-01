// PR c++/52725

struct A { };

const int n = 42;

void f()
{
  A** p = new (A*[n]);
}
