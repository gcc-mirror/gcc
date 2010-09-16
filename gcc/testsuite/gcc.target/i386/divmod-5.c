/* { dg-do compile } */
/* { dg-options "-O2 -m8bit-idiv" } */

extern void foo (int, int, int, int, int, int);

void
bar (int x, int y)
{
  foo (0, 0, 0, 0, x / y, x % y);
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "idivl" 1 } } */
