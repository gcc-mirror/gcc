// test that contracts on overriding functions are found correctly
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on -fcontracts-nonattr" }

#include <cstdio>

struct T {
  int x;
  T (int _x) : x(_x) {}
  ~T () { x = -1; }
};

struct Base
{
  virtual T f(const int a) pre (a > 5);
};

T Base::f(const int a)
{
  return T (a);
}

// inherits original
struct Child0 : Base
{
};

struct Child1 : Base
{
  virtual T f(const int a) pre (a > 14){ return T (a + 10); }
};


struct GChild1 : Child0
{
  virtual T f(const int a) post(a > 6) { return T (a + 100); };
};

struct GChild2 : Child1
{
  virtual T f(const int a) post(a > 30) { return T (a + 200); };
};

T fooBase(Base& b)
{
    return b.f(1);
}

int main(int, char**)
{
  Base b;
  Child0 c0;
  Child1 c1;
  GChild1 g1;
  GChild2 g2;
  T q = b.f(3);     // a > 5
  printf("Base: %d\n", q.x );
  printf("Child0: %d\n", c0.f(3).x);  // a > 5
  printf("Child1: %d\n", c1.f(7).x);  // a > 14
  printf("GChild1: %d\n", g1.f(3).x);  // a > 6
  printf("GChild2: %d\n", g2.f(7).x);  // a > 30

  printf("fooBase(Base): %d\n", fooBase(b).x);     // a > 5
  printf("fooBase(Child0): %d\n", fooBase(c0).x);     // a > 5
  printf("fooBase(Child1): %d\n", fooBase(c1).x);     // a > 14
  printf("fooBase(GChild1): %d\n", fooBase(g1).x);     // a > 6
  printf("fooBase(GChild2): %d\n", fooBase(g2).x);     // a > 30
  return 0;
}

// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "Base: 3(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "Child0: 3(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child1::f at .*: a > 14.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child1::f at .*: a > 14.*(\n|\r\n|\r)" }
// { dg-output "Child1: 17(\n|\r\n|\r)" }
// { dg-output "contract violation in function GChild1::f at .*: a > 6.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function GChild1::f at .*: a > 6.*(\n|\r\n|\r)" }
// { dg-output "GChild1: 103(\n|\r\n|\r)" }
// { dg-output "contract violation in function GChild2::f at .*: a > 30.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function GChild2::f at .*: a > 30.*(\n|\r\n|\r)" }
// { dg-output "GChild2: 207(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "fooBase.Base.: 1(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "fooBase.Child0.: 1(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child1::f at .*: a > 14.*(\n|\r\n|\r)" }
// { dg-output "fooBase.Child1.: 11(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function GChild1::f at .*: a > 6.*(\n|\r\n|\r)" }
// { dg-output "fooBase.GChild1.: 101(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function GChild2::f at .*: a > 30.*(\n|\r\n|\r)" }
// { dg-output "fooBase.GChild2.: 201(\n|\r\n|\r)" }
