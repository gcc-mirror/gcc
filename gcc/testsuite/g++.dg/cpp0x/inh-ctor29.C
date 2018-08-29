// PR c++/67054
// { dg-do compile { target c++11 } }

struct A
{
  A(int) {}
};

struct C
{
  C(int) {}
};

struct B : A
{
  using A::A;
  C c = 42;
};

int main()
{
  B b = 24;
}
