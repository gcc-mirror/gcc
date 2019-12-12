/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
optimize_me_not ()
{
  __asm__ volatile ("" : : : "memory");
}
volatile int a;
static short b[3][9][1] = {5};
int c;
int main() {
    int i, d;
    i = 0;
    for (; i < 3; i++) {
	c = 0;
	for (; c < 9; c++) {
	    d = 0;
	    for (; d < 1; d++)
	      a = b[i][c][d];
	}
    }
    i = c = 0;
    for (; c < 7; c++)
      for (; d < 6; d++)
	a;
    /* i may very well be optimized out, so we cannot test for i == 0.
       Instead test i + 1 which will make the test UNSUPPORTED if i
       is optimized out.  Since the test previously had wrong debug
       with i == 2 this is acceptable.  Optimally we'd produce a
       debug stmt for the final value of the loop which would fix
       the UNSUPPORTED cases.  */
    optimize_me_not(); /* { dg-final { gdb-test . "i + 1" "1" } } */
}
