// PR c++/87109
// { dg-do run { target c++11 } }

#include <utility>

struct C { int i; };
struct D { int i; };

struct A {
  int j;
  operator C() & { return { 1 }; }
  operator C() && { return { 2 }; }
};

struct B : public A {
  operator D() & { return { 3 }; }
  operator D() && { return { 4 }; }
};

C
f (A a)
{
  return a;
}

C
f2 (A a)
{
  return std::move (a);
}

C
f3 ()
{
  A a;
  return a;
}

C
f4 ()
{
  A a;
  return std::move (a);
}

C
f5 ()
{
  return A();
}

D
f6 (B b)
{
  return b;
}

D
f7 (B b)
{
  return std::move (b);
}

D
f8 ()
{
  B b;
  return b;
}

D
f9 ()
{
  B b;
  return std::move (b);
}

D
f10 ()
{
  return B();
}

int
main ()
{
  C c1 = f (A());
  if (c1.i != 2)
    __builtin_abort ();
  C c2 = f2 (A());
  if (c2.i != 2)
    __builtin_abort ();
  C c3 = f3 ();
  if (c3.i != 2)
    __builtin_abort ();
  C c4 = f4 ();
  if (c4.i != 2)
    __builtin_abort ();
  C c5 = f5 ();
  if (c5.i != 2)
    __builtin_abort ();
  D c6 = f6 (B());
  if (c6.i != 4)
    __builtin_abort ();
  D c7 = f7 (B());
  if (c7.i != 4)
    __builtin_abort ();
  D c8 = f8 ();
  if (c8.i != 4)
    __builtin_abort ();
  D c9 = f9 ();
  if (c9.i != 4)
    __builtin_abort ();
  D c10 = f10 ();
  if (c10.i != 4)
    __builtin_abort ();
}
