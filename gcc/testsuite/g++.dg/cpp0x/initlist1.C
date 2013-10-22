// Basic uses of initializer lists
// { dg-do run }
// { dg-options "-std=c++11" }

#include <initializer_list>

extern "C" void abort();

using namespace std;

struct A { int i,j; A(int _i,int _j): i(_i), j(_j) {} };
struct B { A a; B(A _a): a(_a) {} };
struct C { B b; C(B _b): b(_b) {} };

struct D
{
  int ia[3];
  D (initializer_list<int> l)
  {
    const int *p = l.begin();
    for (int i = 0; i < 3; ++i)
      ia[i] = *p++;
  }
};

void f(C c)
{
  if (c.b.a.i != 1) abort();
  if (c.b.a.j != 2) abort();
}
void f(int);

void g(D d)
{
  if (d.ia[0] != 1 || d.ia[1] != 2 || d.ia[2] != 3)
    abort();
}

struct E
{
  int i, j, k;
};

void h(E e)
{
  if (e.i != 1 || e.j != 2 || e.k != 3)
    abort();
}

void i(initializer_list<int> l)
{
  const int *p = l.begin();
  if (*p++ != 1) abort();
  if (*p++ != 2) abort();
  if (*p++ != 3) abort();
  if (p != l.end()) abort();
}

struct U { U(int, int) {} };
U ua[] = { { 3, 2 } };

int main()
{
  g({1,2,3});

  h({1,2,3});

  f({{{1,2}}});
  f({{A{1,2}}});

  i({1,2,3});
}
