// PR c++/91607
// { dg-do compile { target c++11 } }

enum A { a };
constexpr A f(const A x) { return x; }

int main()
{
  constexpr A a0 = f(a);
  constexpr A a1 {};
  constexpr A a2 = f(a1);
}
