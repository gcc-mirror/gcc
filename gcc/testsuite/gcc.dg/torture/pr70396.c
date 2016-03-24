/* { dg-do compile } */

struct S2 {
    signed f1 : 3;
};
int a[100];
int b, c;
char d;
void fn1() {
    struct S2 e;
    b / e.f1;
    c = 2;
    for (; c < 100; c++) {
	d = 0;
	a[c] = ~e.f1 != d;
    }
}
