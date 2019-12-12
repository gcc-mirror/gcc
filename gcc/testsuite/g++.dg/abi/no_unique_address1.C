// { dg-do run { target c++2a } }

struct B { };

struct A
{
  [[no_unique_address]] B b;
  int i;
};

struct C
{
  B b;
  int i;
};

struct D: B { };

struct E
{
  B b [[no_unique_address]];
  D d [[no_unique_address]];
};

constexpr bool same (void *x, void *y) { return x == y; }

int main()
{
  A a;
  if (!same(&a.b, &a.i))
    __builtin_abort();
  C c;
  if (same(&c.b, &c.i))
    __builtin_abort();
  E e;
  if (same (&e.b, &e.d))
    __builtin_abort();
}
