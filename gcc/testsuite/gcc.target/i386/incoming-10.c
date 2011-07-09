/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ia32 } } } */
/* { dg-options "-w -mstackrealign -fomit-frame-pointer -O3 -march=barcelona -mpreferred-stack-boundary=4" } */

struct s {
	int x[8];
};

void g(struct s *);

void f()
{
	int i;
	struct s s;
	for (i = 0; i < sizeof(s.x) / sizeof(*s.x); i++) s.x[i] = 0;
	g(&s);
}

/* { dg-final { scan-assembler "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
