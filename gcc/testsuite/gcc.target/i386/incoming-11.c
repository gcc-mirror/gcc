/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ilp32 } } } */
/* { dg-options "-w -mstackrealign -fomit-frame-pointer -O3 -march=barcelona -mpreferred-stack-boundary=4" } */

void g();

int p[100];
int q[100];

void f()
{
	int i;
	for (i = 0; i < 100; i++) p[i] = 0;
	g();
	for (i = 0; i < 100; i++) q[i] = 0;
}

/* { dg-final { scan-assembler "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
