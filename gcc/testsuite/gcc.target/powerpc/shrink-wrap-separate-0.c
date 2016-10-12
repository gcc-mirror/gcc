/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler {#before\M.*\mmflr\M} } } */

/* This tests if shrink-wrapping for separate components works.

   r20 (a callee-saved register) is forced live at the start, so that we
   get it saved in a prologue at the start of the function.
   The link register only needs to be saved if x is non-zero; without
   separate shrink-wrapping it would however be saved in the one prologue.
   The test tests if the mflr insn ends up behind the prologue.  */

void g(void);

void f(int x)
{
	register int r20 asm("20") = x;
	asm("#before" : : "r"(r20));
	if (x)
		g();
	asm(""); // no tailcall of g
}
