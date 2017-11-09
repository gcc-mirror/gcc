/* { dg-do compile } */
/* { dg-options "-O2 -mforce-indirect-call" } */
/* { dg-final { scan-assembler-times "call\[ \\t\]+\\*%" 2 } } */
/* { dg-final { scan-assembler-times "jmp\[ \\t\]+\\*%" 1 } } */
int x;
int y;

void __attribute__((noinline)) f1(void)
{
	x++;
}

static __attribute__((noinline)) void f3(void)
{
	y++;
}

void f2()
{
	f1();
	f3();
	f1();
}
