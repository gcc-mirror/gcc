// PR c++/44358
// { dg-options "-std=c++0x" }

#include <initializer_list>

struct A
{
  A(int);
};

struct B
{
  B(std::initializer_list<A>);
};

void f (B b);
int main()
{
  B b0 = {{1}};
  B b1 = {{1.0}};		// { dg-error "narrowing" }
}
