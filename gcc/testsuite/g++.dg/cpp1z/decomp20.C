// { dg-do compile { target c++17 } }

struct A { int i,j; };

A f();

int main()
{
  auto [i,j] (f());
}
