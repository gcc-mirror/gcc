// PR c++/20142
// { dg-do run }

int n=4;

struct A
{
  A() {}
  A& operator= (const A&) { --n; return *this; }
};

struct B
{
  A x[2][2];
};

int main()
{
  B b;
  b = b;
  return n;
}
