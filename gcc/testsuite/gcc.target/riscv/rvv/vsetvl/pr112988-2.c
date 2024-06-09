/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -fno-vect-cost-model" } */

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

  if (a != 0 || q != 0 || p != 1 || r != 4 || x != 0xE6 || d != 0)
    __builtin_abort ();

  return 0;
}
