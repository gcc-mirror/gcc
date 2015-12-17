/* { dg-do compile } */
/* { dg-additional-options "-fno-vect-cost-model" } */

int printf (const char *, ...);

int a, b, g;
short c, e, h, i;
int f[8];
void fn1() {
    short j;
    for (; a;) {
	printf("%d", g);
	b = 7;
	for (; b >= 0; b--) {
	    i = 1;
	    short k = f[b];
	    e = k ? k : 3;
	    j = (i && (c |= e)) << 3;
	    int l = j, m = 0;
	    h = l < 0 || l >> m;
	    f[b] = h;
	}
    }
}
