// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Jan 2003 <nathan@codesourcery.com>

// PR 9433. segfault at runtime.

struct A        { virtual void f() {}; };
struct B        {int b;};
struct C : A, B { virtual void f() {}; int c;};
struct D : C    {int d;};
struct E : C    {int e;};
struct F : protected D, E  {int f;};
struct H : virtual F {int h;};
struct I : H  {int i;};
struct J : H  {int j;};
struct K : I, J { virtual void f() {}; int k; };
struct M : K  {int m;};
struct N : M  {int n;};
struct O : M  {int o;};
struct P : N, O { virtual void f() {}; int p;};

int main()
{
  P obj;
  A* a1 = (D *) (&obj);
  H* hp = dynamic_cast<H*>(a1);
  return hp != 0;
}
