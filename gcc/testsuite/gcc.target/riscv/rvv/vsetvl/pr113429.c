/* { dg-do run } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3" } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target riscv_v } */

long a;
int b, c, d, e, f, g;
short h, i, j;
static int k = 3;
static int l = 6;
int m[5][7];
signed char n;
int *const o = &c;

signed char(p)(signed char p1, signed char q) {
  return p1 / q;
}

void s(unsigned p1) {
  b = (b ^ p1) & 255;
}

static long t() {
  long u;
  signed char v;
  d = 1;
  for (; d <= 4; d++) {
    j = 0;
    for (; j <= 4; j++) {
      v = 0;
      for (; v <= 4; v++) {
        if (m[v][v])
          continue;
        c = 0;
        for (; c <= 4; c++) {
          n = 0;
          for (; n <= 4; n++) {
            int *w = &e;
            long r = v;
            u = r == 0 ? a : a % r;
            h |= u;
            *w = g;
            --m[n][c];
            f &= *o;
          }
        }
        if (p((i < 3) ^ 9, k))
          ;
        else if (v)
          return 0;
      }
    }
  }
  return 1;
}

static char x() {
  for (;;) {
    t();
    if (l)
      return 0;
  }
}

int main() {
  x();
  s(e & 255);
  if (b == 0)
    return 0;
  else
    return 1;
}
