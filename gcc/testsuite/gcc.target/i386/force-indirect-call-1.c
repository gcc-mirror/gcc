/* { dg-do compile } */
/* { dg-options "-O2 -mforce-indirect-call" } */
/* { dg-final { scan-assembler-times "(?:call|jmp)\[ \\t\]+\\*%" 3 } } */

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
