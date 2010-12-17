/* { dg-do compile } */
/* { dg-options "-O2 -m8bit-idiv" } */

extern void abort (void);

void
test (int x, int y, int q, int r)
{
  if ((x / y) != q || (x % y) != r)
    abort ();
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "idivl" 1 } } */
