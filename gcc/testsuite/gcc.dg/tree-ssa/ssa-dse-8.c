/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce -fno-tree-ccp -fno-tree-copy-prop -fno-tree-dominator-opts" } */

/* This caused the compiler to enter an infinite loop if copies are not
   fully propagated.   The options are to disable copy propagation and
   thus expose the bug.   */

int foo (void);

struct A {
  struct B {
    struct B *n;
  } *p;
};

static inline void baz (struct A *a)
{
  a->p = a->p->n;
}

void bar (struct A a)
{
  while (foo ())
    baz (&a);
  while (foo ());
}
