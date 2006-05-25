// PR c++/20103
// { dg-options "" }

struct A
{
  A(const A&);
};

struct B
{
  A a;
};

void foo(B);

void bar(A &x)
{
  foo((B){x});
}
