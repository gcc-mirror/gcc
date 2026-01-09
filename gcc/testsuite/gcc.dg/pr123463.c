/* { dg-do compile } */

enum mm { L };
extern const enum mm m[1];
void f(unsigned egno) {
	f(m);		/* { dg-error "integer from pointer" } */
}

