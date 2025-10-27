/* { dg-do compile } */

int a, c, e, f, h, j;
long g, k;
void *malloc(long);
void free(void *);
int b(int m) {
  if (m || a)
    return 1;
  return 0.0f;
}
int d(int m, int p2) { return b(m) + m + (1 + p2 + p2); }
int i() {
  long l[] = {2, 9, 7, 8, g, g, 9, 0, 2, g};
  e = l[c] << 6;
}
void n() {
  long o;
  int *p = malloc(sizeof(int));
  k = 1 % j;
  for (; i() + f + h; o++)
    if (p[d(j + 6, (int)k + 1992695866) + h + f + j + (int)k - 1 + o])
      free(p);
}
