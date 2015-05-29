#include <stdlib.h>
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-release_ssa"  } */
namespace {
struct A
{ int a; virtual int foo() {return a;} void bar() {a=7;} };
struct B
{ int b; virtual int foo2() {return b;} void bar2() {b=9;} };
struct C : public virtual A, public virtual B { };
struct D : public virtual B, public virtual A { };
struct E : public C, public D { void bar2() {b=9;} }; }
int
main(void)
{
  struct E e;
  struct C *c = &e;
  struct D *d = &e;
  struct A *a = &e;
  struct B *b = &e;
  e.bar();
  e.bar2();
  if (e.foo() + e.foo2() != 16)
    abort ();
  if (c->foo() + d->foo2() != 16)
    abort ();
  if (a->foo() + b->foo2() != 16)
    abort ();
  return 0;
}
/* { dg-final { scan-tree-dump-not "abort" "release_ssa"  } } */
