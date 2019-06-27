/* Test LOOP recognition */

/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

unsigned int
test_loop (unsigned int n)
{
  unsigned int i;
  volatile unsigned int s = 0;

  if (n >= 0x10000) return 0;

  /* { dg-final { scan-assembler "loop\\t.L\[0-9\]*, r\[0-9\]*" } } */
  for (i = 0; i < n; i++)
    s++;
  return s;
}
