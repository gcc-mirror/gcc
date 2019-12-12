/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
optimize_me_not ()
{
  __asm__ volatile ("" : : : "memory");
}
volatile long a;
int b[9][1];
static short c[2][1] = {3};
int main()
{
  int i, d, e;
  i = 0;
  for (; i < 9; i++)
    a = b[i][0];
  i = 0;
  for (; i < 2; i++)
    {
      d = 0;
      for (; d < 1; d++)
	{
	  e = 0;
	  for (; e < 1; e++)
	    a = c[i][e];
	  /* i may very well be optimized out, so we cannot test for i == 0.
	     Instead test i + 1 which will make the test UNSUPPORTED if i
	     is optimized out.  Since the test previously had wrong debug
	     with i == 9 this is acceptable.  */
	  optimize_me_not(); /* { dg-final { gdb-test . "i + 1" "1" } } */
	}
    }
  return 0;
}
