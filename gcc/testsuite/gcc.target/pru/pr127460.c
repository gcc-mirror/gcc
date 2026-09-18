/* Verify that debug instructions do not affect repeat-loop padding.  */

/* { dg-options "-O1 -mloop -g -fno-var-tracking" } */

unsigned int
test_loop (unsigned int n, unsigned int x)
{
  unsigned int i;

  if (n >= 0x10000)
    return 0;
  if (!n)
    return 0;

  /* { dg-final { scan-assembler "loop\\t.L\[0-9\]*, r\[0-9\]*" } } */
  /* { dg-final { scan-assembler-times {\tnop} 1 } } */
  for (i = 0; i < n; i++)
    x <<= 2;
  return x;
}
