// PR c++/33208

struct A
{
  bool b;
};

void f(A a)
{
  a.b--; // { dg-error "5:use of an operand of type .bool." }
}
