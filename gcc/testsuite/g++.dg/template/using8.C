// { dg-do compile }

// Origin: Sergey Shandar <comer@pisem.net>

// PR c++/9810: Access checking for member function template
// appeared in using declaration.

struct A
{
  template<class R> void F(R) {}
};

struct B: private A
{
  using A::F;
};

int main()
{
  B b;
  b.F(3);
}
