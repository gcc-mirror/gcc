/* Test that LOOP will not be generated when body contains asm statement */

/* { dg-do compile } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

unsigned int
test_loop (unsigned int n)
{
	unsigned i;
	/* { dg-final { scan-assembler-not "loop\t.\+" } } */
	for (i = 0; i < 10; i++) {
		n <<= 2;
		asm volatile ("nop" : : );
	}
	return n;
}
