/* Test LOOP recognition - short ints*/

/* { dg-options "-O2 -mloop" } */

/* -O2 in the options is significant.  Without it do-loop will not be
   run.  */

unsigned int
test_loop_short (int x, short n)
{
  int i;

  /* { dg-final { scan-assembler "loop\\t.L\[0-9\]*, r\[0-9w.\]*" } } */
  for (i = 0; i < n; i++)
    x <<= 3;
  return x;
}
