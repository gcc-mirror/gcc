/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -frename-registers -mrvv-max-lmul=m8" } */

signed char e;
short f = 8;
signed d;
int(g)(int o, int r) { return o & (o ^ -1) < 0 ? o : o - r; }
#pragma pack(1)
struct {
  short h;
  unsigned : 18;
  short i;
  long j;
  int k;
  char l;
  long m;
  int n;
} a, b, s, c, *p = &b, *u = &s, q = {1};
void t() {
  *p = a;
  for (; e > -7; e = g(e, 8))
    ;
  q = *u = c;
  for (; d - 3; d = 3)
    ;
}

/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,\s*32} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
