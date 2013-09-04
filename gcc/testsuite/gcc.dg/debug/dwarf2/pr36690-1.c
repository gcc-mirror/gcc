/* PR debug/36690 */
/* Verify that break func is hit.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf -dA" } */

int i;

void
func (void)
{
  while (i == 1)
    i = 0;
}

int
main (void)
{
  func ();
  return 0;
}

/* { dg-final { scan-assembler "pr36690-1.c:11" } } */
