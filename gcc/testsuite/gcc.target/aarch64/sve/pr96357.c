/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve" } */

int d;

void
f1(char f, char *g, char *h, char *l, char *n) {
  double i = d, j = 1.0 - f, k = j ? d : j;
  if (k == 1.0)
    i = 0.0;
  *l = *n = *g = *h = i * 0.5;
}

void
f2() {
  int a, m, c;
  for (c = 2048; c; c--) {
    char b = a++;
    f1(b, m, m + 1, m + 2, m + 3); /*{ dg-warning {passing argument [0-9]+ of 'f1' makes pointer from integer without a cast} } */
    m += 4;
  }
}

/* { dg-final { scan-assembler {\tmovprfx\tz[0-9]+, z[0-9]+} } } */
/* { dg-final { scan-assembler {\tfsubr\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #1.0} } } */
