/* Validate that r3 may be used as the literal pool pointer.  Test that only on
   64-bit for z900 to simplify the test.  It's not really different on 31-bit
   or other cpus.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-march=z900 -O2" } */

__int128 gi;
const int c = 0x12345678u;
int foo(void)
{
	gi += c;
	return c;
}

/* { dg-final { scan-assembler-times "\tlarl\t%r3,.L\[0-9\]" 1 } } */
