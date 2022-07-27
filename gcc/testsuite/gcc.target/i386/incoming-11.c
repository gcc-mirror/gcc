/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ia32 } } } */
/* { dg-options "-w -mstackrealign -fomit-frame-pointer -O3 -march=barcelona -mpreferred-stack-boundary=4" } */

void g();

int p[100];
int q[100];

void f()
{
	int i;
	for (i = 0; i < 100; i++) p[i] = 1;
	g();
	for (i = 0; i < 100; i++) q[i] = 1;
}

/* { dg-final { scan-assembler-not "andl\[\\t \]*\\$-16,\[\\t \]*%esp"  { xfail *-*-* } } } */
