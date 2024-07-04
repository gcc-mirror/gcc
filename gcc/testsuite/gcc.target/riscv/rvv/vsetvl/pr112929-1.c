/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

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
}

/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\tzero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {li\t[a-x0-9]+,\s*32} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
