/* PR rtl-optimization/127661 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-thread-jumps" } */

int r;

__attribute__ ((noipa)) void a (void) { r |= 1; }
__attribute__ ((noipa)) void b (void) { r |= 2; }
__attribute__ ((noipa)) void c (void) { r |= 4; }

/* Jump bypassing moves the edge out of the call to A past the test of V
   and then past the test of W.  Each step must use the constant of the
   register that it tests.  */

__attribute__ ((noipa)) void
foo (int v, int w, int s)
{
  if (s)
    {
      v = 7;
      w = 3;
      a ();
    }
  if (v == 3)
    b ();
  if (w == 7)
    c ();
}

int
main ()
{
  foo (3, 7, 1);
  if (r != 1)
    __builtin_abort ();
  r = 0;
  foo (3, 7, 0);
  if (r != 6)
    __builtin_abort ();
  return 0;
}
