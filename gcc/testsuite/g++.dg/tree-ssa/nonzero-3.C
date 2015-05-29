/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdelete-null-pointer-checks" } */
struct A {int a;};
struct B {int b;};
struct C:A,B {int c;
  void bar();};

void foo (struct B *);
void C::bar ()
{
  struct C *d = this;
  foo(this);
  foo(d);
}
void bar (struct C &c)
{
  struct C *d = &c;
  foo(&c);
  foo(d);
}
/* { dg-final { scan-tree-dump-not "if \\(" "vrp1"} } */
