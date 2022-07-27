/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize -fno-vect-cost-model" } */

extern char a[];
char *b;
void e() {
  char *d;
  int c;
  d = a;
  for (; c; c++) {
    d[2] = d[1] = d[0] = b[c];
    d += 3;
  }
}
