/* { dg-do compile } */
/* { dg-options "-O3 -fstrict-enums -fno-inline" } */

enum a {};
int *d;
int b, e, f;
a c, g;
class h {
    virtual unsigned i();
};
class j : h {
    unsigned i() {
	for (;;) {
	    b = c <= 0;
	    if (b)
	      e = *d;
	    b = g && c;
	    if (b)
	      f = *d;
	}
    }
};
void k() { new j; }
