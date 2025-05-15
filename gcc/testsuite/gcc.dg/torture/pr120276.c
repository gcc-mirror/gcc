/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve" { target aarch64*-*-* } } */

int a;
char b[1];
int c[18];
void d(char *);
void e() {
  int f;
  char *g;
  a = 0;
  for (; a < 18; a++) {
    int h = f = 0;
    for (; f < 4; f++) {
      g[a * 4 + f] = c[a] >> h;
      h += 8;
    }
  }
  d(b);
}