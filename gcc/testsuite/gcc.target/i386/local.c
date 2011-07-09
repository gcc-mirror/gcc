/* { dg-do compile } */
/* { dg-options "-O2 -funit-at-a-time" } */
/* { dg-final { scan-assembler "magic\[^\\n\]*eax" { target ia32 } } } */
/* { dg-final { scan-assembler "magic\[^\\n\]*edi" { target { ! { ia32 } } } } } */

/* Verify that local calling convention is used.  */
static t(int) __attribute__ ((noinline));
extern volatile int i;

void m(void)
{
	t(i);
}

static t(int a)
{
	asm("magic %0"::"g"(a));
}
