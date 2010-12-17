/* { dg-do run } */
/* { dg-options "(-mips16)" } */

extern void abort (void);

__complex float f = { -1.0 + -1.0i };
__complex float __attribute__((nomips16)) foo (void) { return f; }
__complex float (*volatile foop) (void) = foo;
__complex float __attribute__((mips16, noinline)) bar (void) { return foop (); }

int
main (void)
{
  if (bar () != f)
    abort ();
  return 0;
}
