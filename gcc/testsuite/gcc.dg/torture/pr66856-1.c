/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */

short c;
int d;
int fn1(int p1, int p2) {
    int a, b;
    a = p1 >> 3 & p2;
    b = p1 & 072;
    a |= a >> 5;
    a |= b >> 5;
    return a;
}
void fn2() {
    short *e = &c;
    int *f;
    int g;
    while (d -= 4) {
	fn1(1, 1);
	fn1(1, 1) * fn1(1, 1) * fn1(1, 1);
	*e++ = fn1(*f++, g);
	*e++ = fn1(*f++, g);
    }
}
