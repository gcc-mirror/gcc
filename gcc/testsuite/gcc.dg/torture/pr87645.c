/* { dg-do compile } */

typedef unsigned a[8];
a b, g;
int c, d, e, f;
int h() {
    unsigned i = 2;
    for (; i < 8; i++)
      b[i] = 0;
    for (; f;) {
	d = 1;
	for (; d < 14; d += 3) {
	    e = 0;
	    for (; e < 8; e++) {
		i = 2;
		for (; i < 8; i++)
		  b[i] = 5 - (c - g[e] + b[i]);
	    }
	}
    }
}
