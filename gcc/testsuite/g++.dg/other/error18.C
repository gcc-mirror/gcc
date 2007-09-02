// PR c++/33208

struct A
{
  bool b;
};

void f(A a)
{
  a.b--; // { dg-error "Boolean expression" }
}
