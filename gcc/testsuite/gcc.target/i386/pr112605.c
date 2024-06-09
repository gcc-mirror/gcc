/* PR target/112605 */
/* { dg-do compile } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack -mforce-indirect-call" } */

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
