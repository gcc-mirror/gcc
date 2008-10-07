/* PR debug/36690 */
/* Verify that break func is hit.  
   This version of the test just checks that it can be compiled, linked
   and executed, further testing is done in corresponding gcc.dg/dwarf2/
   test and hopefully in gdb testsuite.  */
/* { dg-do run } */
/* { dg-options "-O0 -g -dA" } */

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
