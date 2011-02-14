/* { dg-do compile }
/* { dg-options "-O -fno-tree-ccp -fno-tree-fre -ftree-vrp" } */

extern void foo (void) __attribute__((noreturn));
void g (void)
{
  void (*f) (void) = foo;
  f ();
}
