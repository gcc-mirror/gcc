/* PR rtl-optimization/127661 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-thread-jumps" } */

int r;

__attribute__ ((noipa)) void a (void) { r |= 1; }
__attribute__ ((noipa)) void b (void) { r |= 2; }
__attribute__ ((noipa)) void c (void) { r |= 4; }
__attribute__ ((noipa)) void d (void) { r |= 8; }

/* Some targets use one compare for both tests of X.  The edge out of the
   call to A then skips that compare but still needs its result, so jump
   bypassing must put a copy of the compare on the edge.  S > 100 leaves
   flags that give the wrong result for X == 5 if the copy is missing.  */

__attribute__ ((noipa)) void
foo (int x, int s)
{
  if (s > 100)
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

int
main ()
{
  foo (20, 200);
  if (r != 9)
    __builtin_abort ();
  r = 0;
  foo (20, 0);
  if (r != 4)
    __builtin_abort ();
  return 0;
}
