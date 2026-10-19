/* PR rtl-optimization/127661 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fdump-rtl-cprop1" } */

#pragma GCC target "+nocmpbr"

void a (void), b (void), c (void), d (void);

/* Both tests of X use the flags of one compare.  Jump bypassing moves the
   edge out of the call to A past that compare to the second test, so it
   must put a copy of the compare on that edge.  */

void
foo (int x, int s)
{
  if (s)
    {
      x = 5;
      a ();
    }
  if (x == 13)
    b ();
  else if (x > 13)
    c ();
  else
    d ();
}

/* { dg-final { scan-rtl-dump "JUMP-BYPASS" "cprop1" } } */
/* { dg-final { scan-rtl-dump-times "scanning new insn" 1 "cprop1" } } */
