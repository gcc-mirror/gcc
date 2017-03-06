// PR c++/79796
// { dg-do compile { target c++11 } }

struct A
{
  A* p = this;
};

void foo()
{
  A a;
  a = A({});
}
