/* PR debug/36690 */
/* Verify that breakpoint can be put on goto f1, it is hit and
   varz at that spot is defined and contains 5.  Nowhere else
   in the function should be varz in the scope.
   This version of the test just checks that it can be compiled, linked
   and executed, further testing is done in corresponding gcc.dg/dwarf2/
   test and hopefully in gdb testsuite.  */
/* { dg-do run } */
/* { dg-options "-O0 -g -dA" } */

int cnt;

void
bar (int i)
{
  cnt += i;
}

void
foo (int i)
{
  if (!i)
    bar (0);
  else
    {
      static int varz = 5;
      goto f1;
    }
  bar (1);
f1:
  bar (2);
}

int
main (void)
{
  foo (0);
  foo (1);
  return 0;
}
