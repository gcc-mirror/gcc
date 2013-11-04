// PR c++/37740
// { dg-options "-std=c++11" }

struct A
{
  int i;
};

struct B
{
  double d;
  A i;
};

int main()
{
  A a;
  new B{3.2, a};
}
