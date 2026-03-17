/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=sve-only" } */
/* { dg-require-effective-target lp64 } */

char b = 41;
int main() {
  signed char a[31];
#pragma GCC novector
  for (int c = 0; c < 31; ++c)
    a[c] = c * c + c % 5;
  {
    signed char *d = a;
#pragma GCC novector
    for (int c = 0; c < 31; ++c, b += -16)
      d[c] += b;
  }
  for (int c = 0; c < 31; ++c) {
    signed char e = c * c + c % 5 + 41 + c * -16;
    if (a[c] != e)
      __builtin_abort();
  }
}
