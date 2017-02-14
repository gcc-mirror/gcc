// { dg-options -std=c++1z }

struct A { int i,j; };

A f();

int main()
{
  auto [i,j] (f());
}
