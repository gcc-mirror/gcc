/*  PR tree-optimization/12517  */

int f(void);
void g(int);
void h(int a, int b, int c)
{
    int i = f();

    if (b && (i & 4))
	g(i & 8 ? 0 : 1);
    if (a) {
	do {
	    if (i & 8)
		g(0);
	    if (i & 4)
		g(i ? 0 : 1);
	} while (--c);
    }
}
