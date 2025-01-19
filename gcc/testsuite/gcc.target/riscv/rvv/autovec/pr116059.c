/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O2 -std=gnu99" } */

char a;
_Bool b[11] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
int main() {
  _Bool *c = b;
  for (signed d = 0; d < 11; d += 1)
    a = d % 2 == 0 ? c[d] / c[d]
                          : c[d];
  if (a != 1)
    __builtin_abort ();
}
