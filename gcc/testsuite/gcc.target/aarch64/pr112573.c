/* { dg-do compile } */
/* { dg-options "-O2 -fno-section-anchors" } */

char a[100];

void f1 (int x, int y)
{
  *((a + y) + 3) = x;
  *((a + y) + 2) = x;
  *((a + y) + 1) = x;
  *((a + y) + 0) = x;
}

/* { dg-final { scan-assembler-times "strb" 4 } } */
/* { dg-final { scan-assembler-times "adrp" 1 } } */
