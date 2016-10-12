/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler {\mmflr\M.*\mbl\M.*\mmflr\M.*\mbl\M} } } */

/* This tests if shrink-wrapping for separate components puts a prologue
   inside a loop when that is useful.  In this case, it saves the link
   register before each call: both calls happen with probability .10,
   so saving the link register happens with .80 per execution of f on
   average, which is smaller than 1 which you would get if you saved
   it outside the loop.  */

int *a;
void g(void);

void f(int x)
{
	int j;
	for (j = 0; j < 4; j++) {
		if (__builtin_expect(a[j], 0))
			g();
		asm("#" : : : "memory");
		if (__builtin_expect(a[j], 0))
			g();
		a[j]++;
	}
}
