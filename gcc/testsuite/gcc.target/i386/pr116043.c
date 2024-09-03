/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O3" } */
/* { dg-final { scan-assembler-not {(?n)lea.*@gottpoff} } } */

extern __thread int a, c, i, j, k, l;
int *b;
struct d {
  int e;
} f, g;
char *h;

void m(struct d *n) {
  b = &k;
  for (; n->e; b++, n--) {
    i = b && a;
    if (i)
      j = c;
  }
}

char *o(struct d *n) {
  for (; n->e;)
    return h;
}

int q() {
  if (l)
    return 1;
  int p = *o(&g);
  m(&f);
  m(&g);
  l = p;
}
