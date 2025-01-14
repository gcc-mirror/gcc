/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl -fwrapv" } */

signed char a = 9;
int main() {
  for (char e = 0; e < 20; e++)
    for (char f = 0; f < 7; f++)
      a *= 57;

  if (a != 41)
    __builtin_abort ();
}

