// { dg-additional-options -fmodules }

import foo;

struct A { };
struct B { virtual void f() { } };

void g(A* a, B* b)
{
  f (a);
  f (b);
}
