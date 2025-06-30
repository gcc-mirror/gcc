/* { dg-do compile } */
/* { dg-additional-options "-fsigned-char -fno-strict-aliasing -fwrapv" } */

unsigned char sub(unsigned char t, unsigned char u) { return t - u; }
unsigned char mul(unsigned char t, unsigned char u) { return t * u; }
int x(int aa, int ab) {
  return ab >= 32 || aa > 18446744073709551615UL >> ab ? aa : aa << ab;
}
int ag;
int ah = 249;
char ap;
static short ar[5][9];
int *as = &ag;
void bf(char cf) {
  for (; ap <= 8; ap++) {
    (ar[1][7] = mul(x(-1L, sub(cf, 247) / cf), ag <= 0)) || ar[1][4]++;
    *as = ag;
  }
  return;
}
int main() {
  bf(ah);
  if (ar[1][7] != 255)
    __builtin_abort ();
  __builtin_exit (0);
}

