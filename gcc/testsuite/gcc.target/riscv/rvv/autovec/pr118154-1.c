/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-std=gnu99 -Wno-pedantic" } */

long a;
char b;
char c[22][484];
int main() {
  for (int e = 4; e < 33; e++) {
    for (int f = 0; f < 3; f++)
      for (int g = 0; g < 18; g++) {
        c[f][g * 22] = 1;
        a = ({ a > 1 ? a : 1; });
      }
    for (int i = 0; i < 33; i++)
      for (int h = 0; h < 6; h++)
        for (int j = 0; j < 17; j++)
          b = ({ b > 17 ? b : 17; });
  }
  if (c[1][44] != 1)
    __builtin_abort ();
}
