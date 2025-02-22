/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -fwhole-program -mrvv-vector-bits=zvl" } */

long long m;
char f = 151;
char h = 103;
unsigned char a = 109;

int main() {
  for (char l = 0; l < 255 - 241; l += h - 102)
    a *= f;

  m = a;

  if (m != 29)
    __builtin_abort ();

  return 0;
}
