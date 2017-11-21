// PR c++/83045
// { dg-do compile }
// { dg-options "-Wreturn-type -O2" }

void foo (void);

int
bar (int a)
{
  if (a != 0)
    foo ();
}	/* { dg-warning "no return statement in function returning non-void" } */

int
baz (int a)
{
  if (a != 0)
    __builtin_abort ();
}	/* { dg-warning "control reaches end of non-void function" } */
