/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -funit-at-a-time" } */
/* { dg-final { scan-assembler "magic\[^\\n\]*eax" } } */

/* Verify that local calling convention is used.  */
static t(int) __attribute__ ((noinline));
m()
{
	t(1);
}
static t(int a)
{
	asm("magic %0"::"g"(a));
}
