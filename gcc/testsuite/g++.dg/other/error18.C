// PR c++/33208

struct A
{
  bool b;
};

void f(A a)
{
  a.b--; // { dg-error "use of an operand of type .bool." }
}
