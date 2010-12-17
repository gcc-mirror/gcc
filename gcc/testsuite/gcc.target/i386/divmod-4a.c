/* { dg-do compile } */
/* { dg-options "-Os -m8bit-idiv" } */

extern void abort (void);

void
test (int x, int y, int q, int r)
{
  if ((x / y) != q || (x % y) != r)
    abort ();
}

/* { dg-final { scan-assembler-not "divb" } } */
/* { dg-final { scan-assembler-times "idivl" 1 } } */
