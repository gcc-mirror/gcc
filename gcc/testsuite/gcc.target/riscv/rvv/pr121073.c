/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-vector-bits=zvl -fsigned-char -fno-strict-aliasing -fwrapv -Wno-stringop-overflow -Wno-aggressive-loop-optimizations" } */

int a;
unsigned char p[1][21];
void init() {
  for (int s = 0; s < 21; ++s)
    for (int t = 0; t < 21; ++t)
      p[s][t] = 39;
  for (short t = 0; t < 9; t += -5077966496202321318LL + 28071)
    a = p[3][t] && p[2][t];
}
