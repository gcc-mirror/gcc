/* PR tree-optimization/48483 - Construct from yourself w/o warning
   { dg-do compile }
   { dg-options "-Wall" } */

void sink (int);

struct B
{
  int x;
};

struct A
{
  B& b;
  A (B &x): b(x) { }
};

__attribute__ ((noipa)) void test_c0_O0 ()
{
  A a (a.b);        // { dg-warning "'a.A::b' is used uninitialized" }
  sink (a.b.x);
}

__attribute__ ((noipa)) int test_c3_O0 (void)
{
  struct S { int a; } s;
  return s.a;       // { dg-warning "s.test_c3_O0\\\(\\\)::S::a' is used uninitialized" }
}

#pragma GCC optimize ("1")

__attribute__ ((noipa)) void test_c0_O1 ()
{
  A a (a.b);        // { dg-warning "'a.A::b' is used uninitialized" }
  sink (a.b.x);
}

__attribute__ ((noipa)) int test_c3_O1 (void)
{
  struct S { int a; } s;
  return s.a;       // { dg-warning "s.test_c3_O1\\\(\\\)::S::a' is used uninitialized" }
}

#pragma GCC optimize ("2")

__attribute__ ((noipa)) void test_c0_O2 ()
{
  A a (a.b);        // { dg-warning "'a.A::b' is used uninitialized" }
  sink (a.b.x);
}

__attribute__ ((noipa)) int test_c3_O2 (void)
{
  struct S { int a; } s;
  return s.a;       // { dg-warning "s.test_c3_O2\\\(\\\)::S::a' is used uninitialized" }
}
