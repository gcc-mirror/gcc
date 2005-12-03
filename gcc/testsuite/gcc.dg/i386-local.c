/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -funit-at-a-time" } */
/* { dg-final { scan-assembler "magic\[^\\n\]*eax" { target i?86-*-* } } } */
/* { dg-final { scan-assembler "magic\[^\\n\]*edi" { target x86_64-*-* } } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-skip-if "" { x86_64-*-* } { "-m32" } { "" } } */

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
