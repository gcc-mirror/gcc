/* PR debug/29609 */
/* Verify that breakpoint on the break is hit.
   This version of the test just checks that it can be compiled, linked
   and executed, further testing is done in corresponding gcc.dg/dwarf2/
   test and hopefully in gdb testsuite.  */
/* { dg-do run } */
/* { dg-options "-O0 -g -dA" } */

extern void abort (void);

int
foo (void)
{
  int a, i;

  for (i = 1; i <= 10; i++)
    {
      if (i < 3)
	a = 1;
      else
	break;
      a = 5;
    }
  return a;
}

int
main (void)
{
  if (foo () != 5)
    abort ();
  return 0;
}
