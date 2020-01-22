/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

/*
  test that a condition  is propagated  inside an if
*/

void test5 (int x)
{
  extern int foo (int);
  if (x == 0)
    foo (x);
  else if (x == 0 )
    link_error ();
}

void test55 (int x, int y)
{
  int u;
  if (x == 5 && y)
    {
      u = x + 22;
      if (u != 27)
        link_error ();
    }
}

/* There should be not link_error calls, if there is any the
   optimization has failed */
/* ??? Ug.  This one may or may not fail based on how fold decides
   that the && should be emitted (based on BRANCH_COST).  Fix this
   by teaching dom to look through && and register all components
   as true.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" { xfail { ! "alpha*-*-* arm*-*-* aarch64*-*-* powerpc*-*-* cris-*-* hppa*-*-* i?86-*-* mmix-*-* mips*-*-* m68k*-*-* moxie-*-* nds32*-*-* s390*-*-* sh*-*-* sparc*-*-* visium-*-* x86_64-*-* riscv*-*-* or1k*-*-* msp430-*-* pru*-*-*" } } } } */
