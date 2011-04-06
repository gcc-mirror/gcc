/* { dg-do compile } */
/* { dg-options "-O -finline-small-functions --param early-inlining-insns=0 -fdump-tree-einline" } */

int foo0();
void bar0() { foo0(); }
void foobar0() { bar0(); }

void foo1();
void bar1() { foo1(); }
void foobar1() { bar1(); }

#if 0
int foo2();
int bar2() { return foo2(); }
/* The size estimate fails to see that inlining the call statement in bar2
   will make its lhs dead.  */
void foobar2() { bar2(); }
#endif

int foo3();
int bar3() { return foo3(); }
int foobar3() { return bar3(); }

int bar4() { return 0; }
void foobar4() { bar4(); }

int bar5() { return 0; }
int foobar5() { return bar5(); }

/* { dg-final { scan-tree-dump-times "Inlining" 5 "einline" } } */
/* { dg-final { cleanup-tree-dump "einline" } } */
