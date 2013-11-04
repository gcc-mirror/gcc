// PR c++/49996
// { dg-options -std=c++11 }

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
