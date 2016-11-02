// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

class A
{
  A(int);
  friend void f();
};

struct B: A
{
  using A::A;
};

void f()
{
  B b(42);
}
