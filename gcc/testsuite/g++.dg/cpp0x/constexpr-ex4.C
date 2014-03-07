// { dg-do compile { target c++11 } }

struct A
{
  constexpr A(int) { }
  constexpr operator int() { return 1; };
};

template <class T>
struct B
{
  static constexpr A a = A(1);
  int ar[a];
};

B<int> b;
