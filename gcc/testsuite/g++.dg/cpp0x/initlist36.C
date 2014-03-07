// PR c++/44358
// { dg-do compile { target c++11 } }

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
  B b2 {1.0};			// { dg-error "narrowing" }
  A a {1.0};			// { dg-error "narrowing" }
}
