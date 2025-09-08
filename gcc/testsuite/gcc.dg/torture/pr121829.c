/* { dg-do compile } */

int a[1][1];
int *b(int *);
int c() {
  int *d[4];
  int **e = &d[3];
  int f;
  for (; f; f++)
    d[f] = &a[1][0];
  b(*e);
}
int *b(int *g) {
  asm goto("" : : : : h);
  int i[9];
h:
  int f;
  for (f = 0; f < 9; f++)
    i[f] = 1;
  *g = i[4];
}
