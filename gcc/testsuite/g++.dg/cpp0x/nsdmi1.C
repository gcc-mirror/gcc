// { dg-do run }
// { dg-options -std=c++11 }

struct A
{
  int i = 42;
};

struct B
{
  int i = 42;
  B() { }
  B(int i): i(i) { }
};

template <class T, T t>
struct C
{
  T m = t;
};

template <class T, T t>
struct D
{
  T m = t;
  D() { }
  D(T m):m(m) { }
};

int main()
{
  A a1;
  if (a1.i != 42) return 1;
  A a2{};
  if (a2.i != 42) return 2;
  A a3[1];
  if (a3[0].i != 42) return 3;

  B b1;
  if (b1.i != 42) return 3;
  B b2 (24);
  if (b2.i != 24) return 4;

  C<int,3> c1;
  if (c1.m != 3) return 5;
  C<int,5> c2 {};
  if (c2.m != 5) return 6;

  D<int,3> d1;
  if (d1.m != 3) return 7;
  D<int,3> d2 (5) ;
  if (d2.m != 5) return 8;
}
