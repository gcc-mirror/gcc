// PR c++/80084
// { dg-do run { target c++17 } }

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
