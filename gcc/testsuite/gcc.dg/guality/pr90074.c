/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
optimize_me_not ()
{
  __asm__ volatile ("" : : : "memory");
}
char a;
short b[7][1];
int main()
{
  int i, c;
  a = 0;
  i = 0;
  for (; i < 7; i++) {
      c = 0;
      for (; c < 1; c++)
	b[i][c] = 0;
  }
  /* i may very well be optimized out, so we cannot test for i == 7.
     Instead test i + 1 which will make the test UNSUPPORTED if i
     is optimized out.  Since the test previously had wrong debug
     with i == 0 this is acceptable.  Optimally we'd produce a
     debug stmt for the final value of the loop during loop distribution
     which would fix the UNSUPPORTED cases.
     c is optimized out at -Og for no obvious reason.  */
  optimize_me_not(); /* { dg-final { gdb-test . "i + 1" "8" } } */
    /* { dg-final { gdb-test .-1 "c + 1" "2" } } */
  return 0;
}
