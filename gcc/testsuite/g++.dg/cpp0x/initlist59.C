// PR c++/49996
// { dg-do compile { target c++11 } }

struct A
{
  ~A()
  { }
};

struct B
{
  const A& ref;
};

int main()
{
  B* p = new B{A()};
}
