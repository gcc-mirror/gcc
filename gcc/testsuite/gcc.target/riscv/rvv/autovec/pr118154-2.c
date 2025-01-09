/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-std=gnu99 -Wno-pedantic" } */

long a;
signed char b;
long long d;
signed char c[22][22][484];
void m(long long *l, int n) { *l ^= n + (*l >> 2); }
int main() {
  signed char l = 35;
  for (signed char f = 4; f; f++) {
    for (signed g = 0; g < 022; g += 4)
      for (signed char h = 0; h < 022; h++) {
        c[9][g][h * 22 + h] = l;
        a = ({ a > 4095 ? a : 4095; });
      }
    for (int i = 0; i < 22; i += 3)
      for (signed char j = 1; j; j++)
        for (signed char k = 0; k < 022; k++)
          b = ({ b > 19 ? b : 19; });
  }
  for (long f = 0; f < 22; ++f)
    for (long g = 0; g < 22; ++g)
      for (long h = 0; h < 22; ++h)
        for (long i = 0; i < 22; ++i)
          m(&d, c[f][g][h * 2 + i]);
  if (d != 38)
    __builtin_abort ();
}
