/* { dg-do compile } */

short a, f, h;
struct S0 {
    int f0;
} b;
char c, d, e, j, k;
int g;
char fn1(char p1, int p2) { return 7 >> p2 ? p1 : p2; }
void fn2() {
    int l, m, n;
    struct S0 o = {0};
    for (;;) {
	int p = 1, r = e;
	unsigned q = 6;
	l = r == 0 ? q : q % r;
	n = l;
	c = f;
	k = fn1(p, n ^ e);
	char s = k;
	j = s / 6;
	if (j) {
	    int t = d, u = m = d ? t : t / d;
	    h = a || u;
	    b.f0 = h;
	    for (; d;)
	      ;
	} else {
	    b = o;
	    if (d != g)
	      for (;;)
		;
	}
    }
}
