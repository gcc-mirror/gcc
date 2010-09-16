/* { dg-do compile } */
/* { dg-options "-Os -m8bit-idiv" } */

extern void abort (void);

void
test (unsigned int x, unsigned int y, unsigned int q, unsigned int r)
{
  if ((x / y) != q || (x % y) != r)
    abort ();
}

/* { dg-final { scan-assembler-not "divb" } } */
/* { dg-final { scan-assembler-times "divl" 1 } } */
