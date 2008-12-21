/* { dg-options "-O2 -march=5kc" } */
/* { dg-final { scan-assembler-not "\tmadd\t" } } */
/* { dg-final { scan-assembler "\tmul\t" } } */
/* { dg-final { scan-assembler "\taddu\t" } } */

NOMIPS16 void f1 (int *a) { a[0] = a[0] * a[1] + a[2]; }
