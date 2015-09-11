// PR c++/57610
// { dg-do run }

extern "C" void abort();

struct A
{
  A() { }
  A(const A&) { abort(); }
};

struct B : A { };

struct X
{
  operator B() { return B(); }
};

int main()
{
  X x;
  const A& r = x;
}
