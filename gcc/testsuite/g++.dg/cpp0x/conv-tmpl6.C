// { dg-do compile { target c++11 } }

struct A
{
  constexpr A(int) { }
  constexpr operator int() const { return 1; };
};

template <class T, int N>
struct B
{
  static constexpr A a = A(N);
  int ar[a];
};

B<int, 10> b;
