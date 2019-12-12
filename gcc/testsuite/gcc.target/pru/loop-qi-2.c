/* Test LOOP recognition */

/* { dg-options "-O2 -mloop" } */

/* -O2 in the options is significant.  Without it do-loop will not be
   run.  */

unsigned int
test_loop_char (unsigned int x, char n)
{
  int i;

  /* { dg-final { scan-assembler "loop\\t.L\[0-9\]*, r\[0-9b.\]*" } } */
  for (i = 0; i < n; i++)
    x <<= 2;
  return x;
}
