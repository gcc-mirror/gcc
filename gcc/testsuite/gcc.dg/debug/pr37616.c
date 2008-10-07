/* PR debug/37616 */
/* Test that one can put breakpoints onto continue, exitlab and break
   and actually see program reaching those breakpoints.
   This version of the test just checks that it can be compiled, linked
   and executed, further testing is done in corresponding gcc.dg/dwarf2/
   test and hopefully in gdb testsuite.  */
/* { dg-do run } */
/* { dg-options "-O0 -g -dA" } */

extern void abort (void);

int
foo (int parm)
{
  int varj, varm;

  for (varj = 0; varj < 10; varj++)
    {
      if (varj == 5)
	continue;
      if (varj == 7 && !parm)
	goto exitlab;
      if (varj == 9)
	break;
      varm = varj;
    }

exitlab:
  return varm;
}

int
main (void)
{
  if (foo (0) != 6)
    abort ();
  if (foo (1) != 8)
    abort ();
  return 0;
}
