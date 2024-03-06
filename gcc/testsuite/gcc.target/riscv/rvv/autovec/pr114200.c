/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-options { -march=rv64gcv -mabi=lp64d -O3 -fwrapv } } */

short a, e = 1;
_Bool b, d;
short c[300];

int main() {
  for (int f = 0; f < 19; f++) {
    for (int g = 0; g < 14; g++)
      for (int h = 0; h < 10; h++)
        a += c[g] + e;
    b += d;
  }

  if (a != 2660)
    __builtin_abort ();
}
