/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -fno-vect-cost-model" } */

int b, c, d, f, i, a;
int e[1] = {0};
int *g = e;
int *k = e;
int *z;
long h;
int j[5] = {0,0,0,0,0};
void n() {
  if (c) {
    int **l = &z;
    *l = e;
    while (d)
      ;
  }
}
void o() {
  for (; b < 5; b += a) {
    n();
    for (h = 0; h < 5; h++)
      j[h] = 1;
    int m = *e != *g;
    a |= i <= m;
    f = -12;
    for (; f; f++)
      if (*k)
        break;
  }
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli} 1 } } */
