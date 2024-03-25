/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -fno-vect-cost-model" } */

int printf(char *, ...);
int a, l, i, p, q, t, n, o;
int *volatile c;
static int j;
static struct pack_1_struct d;
long e;
char m = 5;
short s;

#pragma pack(1)
struct pack_1_struct {
  long c;
  int d;
  int e;
  int f;
  int g;
  int h;
  int i;
} h, r = {1}, *f = &h, *volatile g;

void add_em_up(int count, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, count);
  __builtin_va_end(ap);
}

int main() {
  int u;
  j = 0;

  for (; j < 9; ++j) {
    u = ++t ? a : 0;
    if (u) {
      int *v = &d.d;
      *v = g || e;
      *c = 0;
      *f = h;
    }
    s = l && c;
    o = i;
    d.f || (p = 0);
    q |= n;
  }

  r = *f;

  add_em_up(1, 1);
  printf("%d\n", m);

  if (m != 5)
    __builtin_abort ();

  return 0;
}
