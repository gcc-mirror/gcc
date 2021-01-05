/* { dg-options "-O2 -ftree-vectorize" } */

int a, b = 5, c = 3;
char d;
char e[1];
int f[] = {0, 0, 1};
short g;
char *h = e;
void i(void) { b = a; }
static void j(void) {
  h = e;
  if (f[2])
  k:
    for (;;) {
      for (c = 0; c <= 4; c++) {
        for (g = 0; g <= 4; g++)
          f[g + 4] &= 2;
      }
      if (d)
        goto k;
    }
}
void l(void) {
  j();
  c = 0;
}
