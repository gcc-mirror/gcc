/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

typedef struct { float x1; } bx;
typedef struct {
    int w;
    short o;
} T2P;
T2P a;
int b;
void fn2();
void fn3(bx*,short);
void fn1() {
    unsigned i = 0;
    int c;
    bx *d;
    bx **h;
    if (b == 0) {
	fn2();
	return;
    }
    for (; c; c++)
      for (; i < 100; i++) {
	  d = h[i];
	  d->x1 = a.w;
      }
    for (; i < 100; i++) {
	d = h[i];
	d->x1 = a.w;
    }
    if (a.o)
      for (; b;)
	fn3(d, a.o);
}
