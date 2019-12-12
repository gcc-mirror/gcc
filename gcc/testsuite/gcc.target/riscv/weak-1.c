/* { dg-do compile } */
/* { dg-options "-mcmodel=medany -mexplicit-relocs -O" } */

/* Verify that the branch doesn't get optimized away.  */
extern int weak_func(void) __attribute__ ((weak));

int
sub (void)
{
  if (weak_func)
    return weak_func ();
  return 0;
}
/* { dg-final { scan-assembler "b\(ne|eq\)" } } */
