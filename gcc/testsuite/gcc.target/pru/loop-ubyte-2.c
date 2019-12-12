/* Test LOOP recognition */

/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

unsigned int
test_loop_ubyte_101 (void)
{
  unsigned int i;
  volatile unsigned int s = 0;

  /* { dg-final { scan-assembler "loop\\t.L\[0-9\]*, 101" } } */
  for (i = 0; i < 101; i++)
    s++;
  return s;
}
