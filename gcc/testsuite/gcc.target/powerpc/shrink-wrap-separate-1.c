/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler {\mmflr\M.*\mbl\M.*\mmflr\M.*\mbl\M} } } */

/* This tests if shrink-wrapping for separate components creates more
   than one prologue when that is useful.  In this case, it saves the
   link register before both the call to g and the call to h.  */

void g(void) __attribute__((noreturn));
void h(void) __attribute__((noreturn));

void f(int x)
{
	if (x == 42)
		g();
	if (x == 31)
		h();
}
