/* Test LOOP generation for very short loops. */

/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

unsigned int
test_loop (unsigned int n, unsigned int x)
{
  unsigned int i;

  if (n >= 0x10000) return 0;
  if (!n) return 0;

  /* { dg-final { scan-assembler "loop\\t.L\[0-9\]*, r\[0-9\]*" } } */
  /* { dg-final { scan-assembler "nop" } } */
  for (i = 0; i < n; i++)
    x <<= 2;
  return x;
}
