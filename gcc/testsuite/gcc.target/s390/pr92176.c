/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzarch" } */

int a = 5, b, c, d, g, h, k, l, m, o;
static int e[7];
int *volatile i = &d;
long long j;

short p(int f, int dummy) {
  k = 0 != (*e = m);
  j = 0;
  for (; j < 59; j = j + 1)
    *i |= b;
  g = 1;
  for (; g <= 4; g++) {
    o = 0;
    for (; o <= 4; o++)
      i = (int * volatile)(long)l;
  }
  return 42;
}

void
q() {
  char *n = (char*)&b;

  (*n = a) == p(e[6], c);
  for (; h;)
    for (;;)
      ;
}

/* { dg-final { scan-assembler-not {(?n)^\tvsteb\t.+,0$} } } */
