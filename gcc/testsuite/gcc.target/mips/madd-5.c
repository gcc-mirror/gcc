/* { dg-options "-O2 -march=5kc" } */
/* { dg-final { scan-assembler-times "\tmadd\t" 4 } } */
/* { dg-final { scan-assembler-not "\tmtlo\t" } } */
/* { dg-final { scan-assembler-times "\tmflo\t" 3 } } */

NOMIPS16 void f1 (int *a) { a[0] = a[0] * a[1] + a[2] * a[3]; }
NOMIPS16 void f2 (int *a) { a[0] = a[0] * a[1] + a[2] * a[3] + a[4]; }
NOMIPS16 void f3 (int *a) { a[0] = a[0] * a[1] + a[2] * a[3] + a[4] * a[5]; }
