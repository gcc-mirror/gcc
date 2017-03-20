// PR c++/80084
// { dg-options -std=c++1z }
// { dg-do run }

struct A
{
  A() { }
  A(const A&) { }
};

struct B
{
  A a;
};

void f(B b)
{
  auto& [a] = b;
  if (&a != &b.a)
    __builtin_abort();
}

int main()
{
  f(B());
}
