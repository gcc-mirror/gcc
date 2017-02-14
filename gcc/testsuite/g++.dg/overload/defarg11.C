// PR c++/78369
// { dg-do compile { target c++11 } }

struct A { };
inline void f(struct A a = {})  {}

int main()
{
  f();
  return 0;
}
