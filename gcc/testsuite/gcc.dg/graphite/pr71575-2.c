/* { dg-do compile } */
/* { dg-options "-Ofast -floop-nest-optimize" } */

int *a;
int b, c, d, e, g;
char f;

void fn1() {
    for (; c;) {
	b = 0;
	for (; b <= 2; b++) {
	    unsigned **h = (unsigned **) &a[b];
	    *h = (unsigned *)(__UINTPTR_TYPE__)((g && (e = d)) != f++);
	}
    }
}
