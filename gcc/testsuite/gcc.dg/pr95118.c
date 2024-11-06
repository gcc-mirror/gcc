/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2 -fdump-tree-fre" } */

void a();
void b() {
    union {
	int c[4];
	long double d;
    } e = {{0, 0, 4}};
    a(e.d);
}
