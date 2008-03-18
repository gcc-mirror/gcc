// PR c++/35548
// { dg-do run }

int c;
struct A
{
  A() { ++c; }
  A(const A&) { ++c; }
  ~A() { --c; }
};

A f()
{
  return A();
}

int i;
const A* ap;
int main()
{
  const A& ar = i ? *ap : f();
  return (c == 0);
}
