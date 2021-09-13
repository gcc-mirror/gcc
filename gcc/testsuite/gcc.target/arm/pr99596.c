/* { dg-options "-Os -mtune=xscale" } */

register int a asm("sp");
extern int b;
typedef struct {
  long c[16 * 8 / 32];
} d;
int e;
int f;
int g;
d h;
int j(int, int, int, d);
int i(void) {
  for (;;) {
    b &&j(e, f, g, h);
    j(e, f, g, h);
  }
}
