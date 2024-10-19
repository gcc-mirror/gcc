/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

int a = 0;
int p, q, r, x = 230;
short d;
int e[256];
static struct f w;
int *c = &r;

short y(short z) {
  return z * d;
}

#pragma pack(1)
struct f {
  int g;
  short h;
  int j;
  char k;
  char l;
  long m;
  long n;
  int o;
} s = {1}, v, t, *u = &v, *b = &s;

void add_em_up(int count, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, count);
  __builtin_va_end(ap);
}

int main() {
  int i = 0;
  for (; i < 256; i++)
    e[i] = i;

  p = 0;
  for (; p <= 0; p++) {
    *c = 4;
    *u = t;
    x |= y(6 >= q);
  }

  *b = w;

  add_em_up(1, 1);

  if (a != 0)
    return 1;
  if (q != 0)
    return 2;
  if (p != 1)
    return 3;
  if (r != 4)
    return 4;
  if (x != 0xE6)
    return 5;
  if (d != 0)
    return 6;

  return 0;
}

/* { dg-final { scan-assembler-times {vsetvli} 5 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\t[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {li\t[a-x0-9]+,\s*32} 2 } } */
