/* PR c/71969 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fno-gnu89-inline -O2 -fdump-tree-einline-details" } */

volatile int v;
#define S v++;
#define S10 S S S S S S S S S S
#define S100 S10 S10 S10 S10 S10 S10 S10 S10 S10 S10

extern inline __attribute__((gnu_inline)) void
foo (void) { S100 }

int
main ()
{
  foo ();
  foo ();
  foo ();
  foo ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Inlining foo/\[0-9\]* into main/\[0-9\]*" 4 "einline" } } */
