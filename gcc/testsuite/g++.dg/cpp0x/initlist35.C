// PR c++/41510
// { dg-do compile { target c++11 } }

struct B
{
  B(int, int);
};
struct A
{
  A(int, int);
  A(const B&);
};

void f()
{
  A a = { 1, 2 };
}

template <class T> void g()
{
  A a = { 1, 2 };
}

template void g<int>();
